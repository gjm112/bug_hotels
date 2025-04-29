#Manuscript: https://loyolauniversitychicago.sharepoint.com/:w:/r/sites/TeamTyphaSP23-WI24/_layouts/15/Doc.aspx?sourcedoc=%7B4DCE1BDD-6EE8-4A4B-A3CE-1A86352890AB%7D&file=Manuscript_WorkingDraft_10_17_24.docx&action=default&mobileredirect=true
library(rjags)
library(ggmosaic)
library(tidyverse)
cong <- read.csv("/Users/gregorymatthews/Dropbox/bug_hotels/data/cong_master.csv")

#Remove the DO outliers
cong <- cong %>% filter(DO_mg_L <= 20)
#Model variable selection.  Must be in model: Tot_veg_pct and Water_depth_cm
#Add Tot_veg_pct and Water_depth_cm and Water_Temp_C and Veg_H (richness and evenness.  This is just entropy)
cong <- cong %>% select(Date, Site, Plot, Week, Treatment, Lat, Long,
                        Inv_abun, Collector, Shredder, Piercer, Scraper, Predator,
                        DO_mg_L,Tot_veg_pct, Water_depth_cm, Water_Temp_C, Veg_H)

cong <- cong %>% mutate(n_col = Inv_abun*Collector,
                        n_shr = Inv_abun*Shredder,
                        n_pie = Inv_abun*Piercer,
                        n_scr = Inv_abun*Scraper,
                        n_pre = Inv_abun*Predator)


cong$Treatment <- as.factor(cong$Treatment)

#Checking
tot <- cong %>% group_by(Treatment) %>% summarize(sum_n_col = sum(n_col),
                                                  sum_n_shr = sum(n_shr),
                                                  sum_n_pie = sum(n_pie),
                                                  sum_n_scr = sum(n_scr),
                                                  sum_n_pre = sum(n_pre)) 
tot[,-1] <- t(apply(tot[,-1],1,function(x){x/sum(x)}))


rownames <- tot$Treatment
tab <-tot[,-1] %>% as.matrix() %>% as.table()
row.names(tab) <- c(rownames)
#Rows and columns are not independent
chisq.test(tab)


#Data viz
ggdat <- cong %>% select(Treatment, n_col, n_shr, n_pie, n_scr, n_pre) %>% pivot_longer(names_to = "Category", cols = c("n_col","n_shr","n_pie","n_scr","n_pre"))
ggplot(aes(fill = Category, x = Treatment, y = value), data = ggdat) + geom_bar(position="fill", stat="identity")

#EDA
names(cong)
table(cong$Date)
table(cong$Site) #All Munuscong
table(cong$Plot)
table(cong$Week)
table(cong$Week, cong$Plot) #Why is MEAD 4 and 5 not repeated?  
table(cong$Treatment)
summary(cong$Inv_abun)
summary(cong$Collector)
summary(cong$DO_mg_L)
summary(cong$Tot_veg_pct)
summary(cong$Water_depth_cm)
summary(cong$Water_Temp_C)

hist(cong$DO_mg_L)
hist(cong$Tot_veg_pct)
hist(cong$Water_depth_cm)
hist(cong$Water_Temp_C)

#Exploratory Data Viz
#Tot Veg Pct and water depth remove these from the model
#Include Veg H
ggplot(aes(x = DO_mg_L, y = Water_depth_cm, color = as.factor(Treatment)), data = cong) + geom_point()
ggplot(aes( y = Water_depth_cm, x = as.factor(Treatment)), data = cong) + geom_boxplot()
ggplot(aes( y = Water_Temp_C, x = as.factor(Treatment)), data = cong) + geom_boxplot()
ggplot(aes( y = Tot_veg_pct, x = as.factor(Treatment)), data = cong) + geom_boxplot()
ggplot(aes( y = DO_mg_L, x = as.factor(Treatment)), data = cong) + geom_boxplot()
ggplot(aes( y = Veg_H, x = as.factor(Treatment)), data = cong) + geom_boxplot()


ggplot(aes( y = Collector, x = DO_mg_L), data = cong) + geom_point() + geom_smooth()
ggplot(aes( y = Collector, x = Water_depth_cm), data = cong) + geom_point() + geom_smooth()

cong %>% group_by(Treatment) %>% summarize()






#Bayes stuff
cong  <- cong %>% mutate(I_meadow = ifelse(Treatment == "meadow",1,0),
                         I_submerged = ifelse(Treatment == "submerged",1,0),
                         I_tyhyd = ifelse(Treatment == "tyhyd",1,0),
                         I_typha = ifelse(Treatment == "typha",1,0),
                         I_week = ifelse(Week == "W1",0,1))

Y <- cong %>% select(n_col, n_shr, n_pie, n_scr, n_pre) 
Y <- as.matrix(Y)
Y <- matrix(as.integer(Y),nrow = nrow(Y), ncol = ncol(Y))
#Creating the design matrix
#Meadow is baseline

################################################################################
#Submerged is a better choice for baseline!!!!
################################################################################
X <- cong %>% select(I_meadow, I_tyhyd, I_typha, DO_mg_L, Water_Temp_C,Veg_H, I_week)
#X <- cong %>% select(I_meadow,I_submerged, I_tyhyd, I_typha,DO_mg_L)
X <- data.frame(int = 1,as.matrix(X))

pairs(X)

#Random effects desing matrix
Z <- matrix(NA, nrow = nrow(cong), ncol = length(unique(cong$Plot)))
for (i in 1:nrow(cong)) {
  for (j in 1:length(unique(cong$Plot))) {
    Z[i, j] <- ifelse(sort(unique(cong$Plot))[j] == cong$Plot[i], 1, 0)
  }
}  
n <- apply(Y, 1, sum)

#Fit a multinomial regression in frequentist world
#Figure out the correct baseline.  
library(nnet)
fit_basic <- nnet::multinom(cbind(n_pre,n_col,n_shr,n_pie,n_scr) ~ I_meadow + I_tyhyd + I_typha + DO_mg_L + Water_Temp_C + Veg_H + I_week, data = cong)
summary(fit_basic)

library(rjags)
####################################
#Fixed and random effects
####################################

data <- list(
  N = nrow(Y), # Number of observations
  J = ncol(Y),        # Number of classes
  Dx = ncol(X),  #Number of columns of X
  Dz = ncol(Z), #Number of columns of Z
  Y = Y,     # Response variable
  X = X,      #Predictor matrix Fixed effects
  Z = Z,     #predictor matrix Random effects
  n = n     #number of total bugs at each site
)

#With predictors
model_string <- "model{

for (i in 1:N){
  Y[i,] ~ dmulti(p[i,], n[i])

for (j in 1:(J-1)){
  p[i,j] <- theta[i,j]/(1+sum(theta[i,]))
}
  p[i,J] <- 1/(1+sum(theta[i,]))

  for (j in 1:(J-1)){
  theta[i,j] <- exp(inprod(X[i,], beta[,j]) + inprod(Z[i,], b[,j]))
  }
 
 
 
}
 
 
for (d in 1:Dx) {
  for (j in 1:(J-1)){
    beta[d, j] ~ dnorm(0, 0.001)
  }
}

for (d in 1:Dz) {
  for (j in 1:(J-1)){
    b[d, j] ~ dnorm(0, tau2b)
  }
}

tau2b <- 1/sigma2b
sigma2b ~  dnorm(0, 0.001)T(0,10000)


}"

#Look at all the paired comparisons. 
#Look at computing evenness score for each draw and then getting a distribution for evenness.  
#compute Shannon-Weaver index which is just entropy.  sum(pi log pi)


setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

# ztest <- jags.samples(jags,c('beta','p','b',"sigma2b"),1000, thin = 1)
# str(ztest)

z <- jags.samples(jags,c('beta','p','b',"sigma2b"),200000, thin = 200)

save(z, file = "/Users/gregorymatthews/Dropbox/bug_hotels/z_20240417.RData")
load("/Users/gregorymatthews/Dropbox/bug_hotels/z_20240417.RData")



#Check convergence. 
i <- 1
j <- 1
conv <- data.frame(iter = rep(1:1000, 3), chain = rep(1:3,each = 1000), beta = c(z$beta[i,j,,1],z$beta[i,j,,2],z$beta[i,j,,3]))
ggplot(aes(x = iter, y = beta, color = factor(chain)),data = conv) + geom_path() 

i <- 1
j <- 1
conv <- data.frame(iter = rep(1:1000, 3), chain = rep(1:3,each = 1000), p = c(z$p[i,j,,1],z$p[i,j,,2],z$p[i,j,,3]))
ggplot(aes(x = iter, y = p, color = factor(chain)),data = conv) + geom_path() 

conv <- data.frame(iter = rep(1:1000, 3), chain = rep(1:3,each = 1000), sigma2b = c(z$sigma2b[1,,1],z$sigma2b[1,,2],z$sigma2b[1,,3]))
ggplot(aes(x = iter, y = sigma2b, color = factor(chain)),data = conv) + geom_path()

#Check ACF plots.
i <- 1
j <- 5
acf(z$sigma2b[1,,1], lag.max = 100)

#summary or results ideas: 

#Plot with 4 rows for each Treamment.  
#And then posterior density plots for each of the categories.
#Using the average level of the random effect.  
str(z$beta)
#z$beta is (9x4)
#X is (62x9)
#baseline response level is predator
#baseline treatment is meadow (This needs to change though I think.)


#Plot the posteriors of beta
#DO_mg_L
dat <- rbind(data.frame(variable = "DO_mg_L",
                  category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each= 1000),
  beta = c(z$beta[5,1,1:1000,1],
             z$beta[5,2,1:1000,1],
             z$beta[5,3,1:1000,1],
             z$beta[5,4,1:1000,1])),

data.frame(variable = "Tot_veg_pct",
           category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each= 1000),
           beta = c(z$beta[6,1,1:1000,1],
                    z$beta[6,2,1:1000,1],
                    z$beta[6,3,1:1000,1],
                    z$beta[6,4,1:1000,1])),

data.frame(variable = "Water_depth_cm",
           category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each= 1000),
           beta = c(z$beta[7,1,1:1000,1],
                    z$beta[7,2,1:1000,1],
                    z$beta[7,3,1:1000,1],
                    z$beta[7,4,1:1000,1])),

data.frame(variable = "Water_temp_C",
           category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each= 1000),
           beta = c(z$beta[8,1,1:1000,1],
                    z$beta[8,2,1:1000,1],
                    z$beta[8,3,1:1000,1],
                    z$beta[8,4,1:1000,1])))
ggplot(aes(x = beta, color = factor(category)), data = dat) + geom_density() + facet_grid(variable~1) + geom_vline(xintercept = 0)


#Find p vectors with average levels 
newX <- X[1:4,]
#Set to 2.5
#newX$DO_mg_L <- mean(X$DO_mg_L)
newX$DO_mg_L <- 2.5
#Set to 50
newX$Tot_veg_pct <- 50
#newX$Tot_veg_pct <- mean(X$Tot_veg_pct)
#Set to 100
#newX$Water_depth_cm <- mean(X$Water_depth_cm)
newX$Water_depth_cm <- 100
#Set to 20
#newX$Water_Temp_C <- mean(X$Water_Temp_C)
newX$Water_Temp_C <- 20
diag(newX[2:4,2:4]) <- 1


#4 treatments, 5 categories
#Posterior density estimates of 
results1 <- results2 <- results3 <- list()
for (i in 1:1000){print(i)
  results1[[i]] <- data.frame(category = rep(c("p_col", "p_shr", "p_pie", "p_scr", "p_pre"),4), treatment = rep(c("meadow","submerged","typhd","typha"),each = 5), p = c(apply(cbind(exp(as.matrix(newX) %*% z$beta[,,i,1]),1),1,function(x){x/sum(x)})))
  results2[[i]] <- data.frame(category = rep(c("p_col", "p_shr", "p_pie", "p_scr", "p_pre"),4), treatment = rep(c("meadow","submerged","typhd","typha"),each = 5), p = c(apply(cbind(exp(as.matrix(newX) %*% z$beta[,,i,2]),1),1,function(x){x/sum(x)})))
  results3[[i]] <- data.frame(category = rep(c("p_col", "p_shr", "p_pie", "p_scr", "p_pre"),4), treatment = rep(c("meadow","submerged","typhd","typha"),each = 5), p = c(apply(cbind(exp(as.matrix(newX) %*% z$beta[,,i,3]),1),1,function(x){x/sum(x)})))
}


results <- rbind(do.call(rbind,results1),do.call(rbind,results2),do.call(rbind,results3))
ggplot(aes(x = p, y = after_stat(scaled), color = category), data = results) + geom_density() + facet_grid(treatment ~ 1)

#Posterior density estimates of betas relative to meadow.
nsim <- 1000
dat <- rbind(data.frame(Treatment = rep("submerged",3*nsim),
                  category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each = 3*nsim),
                  beta = c(z$beta[2,1,1:1000,1],z$beta[2,1,1:1000,2],z$beta[2,1,1:1000,3],
                           z$beta[2,2,1:1000,1],z$beta[2,2,1:1000,2],z$beta[2,2,1:1000,3],
                           z$beta[2,3,1:1000,1],z$beta[2,3,1:1000,2],z$beta[2,3,1:1000,3],
                           z$beta[2,4,1:1000,1],z$beta[2,4,1:1000,2],z$beta[2,4,1:1000,3])), 
             
             data.frame(Treatment = rep("typhd",3*nsim),
                        category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each = 3*nsim),
                        beta = c(z$beta[3,1,1:1000,1],z$beta[3,1,1:1000,2],z$beta[3,1,1:1000,3],
                                 z$beta[3,2,1:1000,1],z$beta[3,2,1:1000,2],z$beta[3,2,1:1000,3],
                                 z$beta[3,3,1:1000,1],z$beta[3,3,1:1000,2],z$beta[3,3,1:1000,3],
                                 z$beta[3,4,1:1000,1],z$beta[3,4,1:1000,2],z$beta[3,4,1:1000,3])),
             
             data.frame(Treatment = rep("typha",3*nsim),
                        category = rep(c("p_col", "p_shr", "p_pie", "p_scr"),each = 3*nsim),
                        beta = c(z$beta[4,1,1:1000,1],z$beta[4,1,1:1000,2],z$beta[4,1,1:1000,3],
                                 z$beta[4,2,1:1000,1],z$beta[4,2,1:1000,2],z$beta[4,2,1:1000,3],
                                 z$beta[4,3,1:1000,1],z$beta[4,3,1:1000,2],z$beta[4,3,1:1000,3],
                                 z$beta[4,4,1:1000,1],z$beta[4,4,1:1000,2],z$beta[4,4,1:1000,3])))

ggplot(aes(x = beta, color = category), data = dat) + geom_density() + facet_grid(Treatment ~ 1)








hist(c(z$beta[5,1,1:1000,1],z$beta[5,1,1:1000,2],z$beta[5,1,1:1000,3]))
hist(c(z$beta[5,2,1:1000,1],z$beta[5,2,1:1000,2],z$beta[5,2,1:1000,3]))
hist(c(z$beta[5,3,1:1000,1],z$beta[5,3,1:1000,2],z$beta[5,3,1:1000,3]))
hist(c(z$beta[5,4,1:1000,1],z$beta[5,4,1:1000,2],z$beta[5,4,1:1000,3]))

#Tot_veg_pct
hist(c(z$beta[6,1,1:1000,1],z$beta[6,1,1:1000,2],z$beta[6,1,1:1000,3]))
hist(c(z$beta[6,2,1:1000,1],z$beta[6,2,1:1000,2],z$beta[6,2,1:1000,3]))
hist(c(z$beta[6,3,1:1000,1],z$beta[6,3,1:1000,2],z$beta[6,3,1:1000,3]))
hist(c(z$beta[6,4,1:1000,1],z$beta[6,4,1:1000,2],z$beta[6,4,1:1000,3]))

hist(c(z$beta[6,1,1:1000,1],z$beta[6,1,1:1000,2],z$beta[6,1,1:1000,3]))
hist(c(z$beta[6,2,1:1000,1],z$beta[6,2,1:1000,2],z$beta[6,2,1:1000,3]))
hist(c(z$beta[6,3,1:1000,1],z$beta[6,3,1:1000,2],z$beta[6,3,1:1000,3]))
hist(c(z$beta[6,4,1:1000,1],z$beta[6,4,1:1000,2],z$beta[6,4,1:1000,3]))

########################################################
#SCRAP
########################################################
#theta[i, J] <- 0 # Linear combination for class J, constrained to be 0
setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

z<-jags.samples(jags,c('beta','p','b',"sigma2b"),50000, thin = 50)

resbeta <- data.frame()
for (i in 2:4){
  for (j in 1:5){
    trt <- case_when(i == 2 ~ "submerged", 
                     i == 3 ~ "tyhyd",
                     i == 4 ~ "typha")
    typ <- case_when(j == 1 ~ "collector",
                      j == 2 ~ "shredder", 
                     j == 3 ~ "piercer",
                     j == 4 ~ "screecher",
                     j == 5 ~ "pre")
  resbeta <- rbind(resbeta,data.frame(Treatment = trt, Type = typ, postbeta = z$beta[j,i,,1]))
  }
}

ggplot(aes(x = postbeta, color = as.factor(Treatment)), data = resbeta) + facet_grid(Type ~ 1) + geom_density()

hist(z$sigma2b[1,1:1000,1])

hist(z$beta[,,,1])






# Define data
data <- list(
  N = nrow(Y), # Number of observations
  J = ncol(Y),        # Number of classes
  Y = Y,     # Response variable
  n = n# Predictor matrix
)

#No covariates 
model_string <- "
model{

for (i in 1:N){
  Y[i,] ~ dmulti(p[],n[i])
  }

for (j in 1:J){
p[j] <- theta[j]/stheta
  theta[j] ~ dgamma(1,1)
}
stheta <- sum(theta)

}"

#theta[i, J] <- 0 # Linear combination for class J, constrained to be 0
setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels.bug")

# Run JAGS
jags_fit <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels.bug",
                       data = data,
                       n.chains = 3, 
                       n.adapt = 100
)


data <- list(
  N = nrow(Y), # Number of observations
  J = ncol(Y),        # Number of classes
  D = ncol(X),
  Y = Y,     # Response variable
  X = X,
  n = n# Predictor matrix
)

#With predictors; fixed effects only 
model_string <- "
model{

for (i in 1:N){
  Y[i,] ~ dmulti(p[i,], n[i])

for (j in 1:J){
  p[i,j] <- theta[i,j]/sum(theta[i,])
  }

  for (j in 1:J){
  theta[i,j] <- exp(inprod(X[i,], beta[,j]))
  }
  
  
}
  
  
for (d in 1:D) {
  for (j in 1:J){
    beta[d, j] ~ dnorm(0, 1) 
  }
}

  


}"



#theta[i, J] <- 0 # Linear combination for class J, constrained to be 0
setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels.bug")

# Specify parameters to monitor
parameters <- c("beta")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

z<-jags.samples(jags,c('beta','p'),50000, thin = 50)

str(z)
plot(z$beta[1,5,1:1000,1])
acf(z$beta[1,5,1:1000,1])


plot(z$beta[1,5,1:1000,1], type = "l")
points(z$beta[1,5,1:1000,2], type = "l", col = "red")
points(z$beta[1,5,1:1000,3], type = "l", col = "blue")


hist(z$beta[5,1,1:100,1])
results <- data.frame(trt = c(rep(c("meadow","submerged","tyhyd","typha"),each = 5000)),
                      cat = c(rep(rep(c("col","shr","pie","scr","pre"), each = 1000),4)),
                      beta = c(z$beta[1,1,1:1000,1],
                               z$beta[1,2,1:1000,1],
                               z$beta[1,3,1:1000,1],
                               z$beta[1,4,1:1000,1],
                               z$beta[1,5,1:1000,1],
                               z$beta[2,1,1:1000,1],
                               z$beta[2,2,1:1000,1],
                               z$beta[2,3,1:1000,1],
                               z$beta[2,4,1:1000,1],
                               z$beta[2,5,1:1000,1],
                               z$beta[3,1,1:1000,1],
                               z$beta[3,2,1:1000,1],
                               z$beta[3,3,1:1000,1],
                               z$beta[3,4,1:1000,1],
                               z$beta[3,5,1:1000,1],
                               z$beta[4,1,1:1000,1],
                               z$beta[4,2,1:1000,1],
                               z$beta[4,3,1:1000,1],
                               z$beta[4,4,1:1000,1],
                               z$beta[4,5,1:1000,1]))

library(ggplot2)
ggplot(aes(x = factor(cat), y = beta, color = factor(trt)), data = results) + geom_boxplot()

#Implied p's
apply(z$beta[1,1:5,1:1000,1],2,function(x){x/sum(x)})[,1]


results <- data.frame(trt = c(rep(c("meadow","submerged","tyhyd","typha"),each = 5000)),
                      cat = c(rep(rep(c("col","shr","pie","scr","pre"), each = 1000),4)),
                      p = c(z$p[1,1,1:1000,1],
                            z$p[1,2,1:1000,1],
                            z$p[1,3,1:1000,1],
                            z$p[1,4,1:1000,1],
                            z$p[1,5,1:1000,1],
                            z$p[2,1,1:1000,1],
                            z$p[2,2,1:1000,1],
                            z$p[2,3,1:1000,1],
                            z$p[2,4,1:1000,1],
                            z$p[2,5,1:1000,1],
                            z$p[3,1,1:1000,1],
                            z$p[3,2,1:1000,1],
                            z$p[3,3,1:1000,1],
                            z$p[3,4,1:1000,1],
                            z$p[3,5,1:1000,1],
                            z$p[4,1,1:1000,1],
                            z$p[4,2,1:1000,1],
                            z$p[4,3,1:1000,1],
                            z$p[4,4,1:1000,1],
                            z$p[4,5,1:1000,1]))

library(ggplot2)
ggplot(aes(x = factor(cat), y = p, color = factor(trt)), data = results) + geom_boxplot()


#Old model formulation

model_string <- "model{
  
  for (i in 1:N){
    Y[i,] ~ dmulti(p[i,], n[i])
    
    for (j in 1:J){
      p[i,j] <- theta[i,j]/sum(theta[i,])
    }
    
    for (j in 1:J){
      theta[i,j] <- exp(inprod(X[i,], beta[,j]) + inprod(Z[i,], b[,j]))
    }
    
    
  }
  
  
  for (d in 1:Dx) {
    for (j in 1:J){
      beta[d, j] ~ dnorm(0, 0.001) 
    }
  }
  
  for (d in 1:Dz) {
    for (j in 1:J){
      b[d, j] ~ dnorm(0, tau2b) 
    }
  }
  
  tau2b <- 1/sigma2b
  sigma2b ~  dnorm(0, 0.001)T(0,10000)
  
  
  
  
}"
 