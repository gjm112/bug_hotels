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
                        DO_mg_L,Tot_veg_pct, Water_depth_cm, Water_Temp_C, Veg_H, Inv_richness, Inv_abun, Inv_biomass_mg)

cong$Treatment <- as.factor(cong$Treatment)

#By treatment
cong %>% group_by(Treatment) %>% summarize(Inv_abun = mean(Inv_abun),
                                                  Inv_biomass_mg = mean(Inv_biomass_mg),
                                                  Inv_richness = mean(Inv_richness)) 



#Data viz
ggplot(aes(x = Treatment, y = Inv_biomass_mg), data = cong) + geom_boxplot()
ggplot(aes(x = Treatment, y = Inv_abun), data = cong) + geom_boxplot()
ggplot(aes(x = Treatment, y = Inv_richness), data = cong) + geom_boxplot()

#EDA
names(cong)
table(cong$Date)
table(cong$Site) #All Munuscong
table(cong$Plot)
table(cong$Week)
table(cong$Week, cong$Plot) #Why is MEAD 4 and 5 not repeated?  
table(cong$Treatment)
summary(cong$Inv_abun)
table(cong$Inv_richness)
table(cong$Inv_richness,cong$Treatment)
summary(cong$Inv_biomass_mg)
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





#Bayes stuff
cong  <- cong %>% mutate(I_meadow = ifelse(Treatment == "meadow",1,0),
                         I_submerged = ifelse(Treatment == "submerged",1,0),
                         I_tyhyd = ifelse(Treatment == "tyhyd",1,0),
                         I_typha = ifelse(Treatment == "typha",1,0),
                         I_week = ifelse(Week == "W1",0,1))

# library(lme4)
# #Linear Model check
# ggplot(aes(x = Plot, y = Inv_biomass_mg), data = cong) + geom_boxplot()
# a <- lmer(Inv_biomass_mg ~ Treatment+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week + (1|Plot) , data = cong)
# summary(a)
# #plot(a)
# 
# b <- lmer(Inv_biomass_mg ~I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week + (1|Plot) , data = cong)
# summary(b)
# #plot(b)
# #I_meadow, I_tyhyd, I_typha, DO_mg_L, Water_Temp_C,Veg_H, I_week
# 
# 
# a <- lmer(Inv_richness ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week + (1|Plot) , data = cong)
# plot(a)
# summary(a)
# 
# a <- lm(Inv_abun ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week  , data = cong)
# a <- lmer(Inv_abun ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week + (1|Plot) , data = cong)
# summary(a)
# plot(a)
# 
# a <- lm((Inv_abun) ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L +  Water_Temp_C+ Veg_H+  I_week , data = cong)
# summary(a)
# plot(a)
# 
# 
# b <- lm(Inv_richness ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week , data = cong)
# plot(b)
# b <- glmer(Inv_richness ~ I_meadow +  I_tyhyd+  I_typha+  DO_mg_L+  Water_Temp_C+ Veg_H+  I_week +(1|Plot) , data = cong, family = "poisson")
# plot(b)
# summary(b)

####################################
#log_Inv_biomass_mg
####################################
Y <- cong %>%  mutate(log_Inv_biomass_mg = log(Inv_biomass_mg)) %>% select(log_Inv_biomass_mg) %>% pull(log_Inv_biomass_mg)
N <- nrow(Y)
#Y <- matrix(as.integer(Y),nrow = nrow(Y), ncol = ncol(Y))
#Creating the design matrix
#Meadow is baseline

################################################################################
#Submerged is a better choice for baseline!!!!
################################################################################
X <- cong %>% select(I_meadow, I_tyhyd, I_typha,DO_mg_L, Water_Temp_C,Veg_H, I_week)
#X <- cong %>% select(I_meadow,I_submerged, I_tyhyd, I_typha,DO_mg_L)
X <- data.frame(int = 1,as.matrix(X))
X <- as.matrix(X)



#Random effects desing matrix
Z <- matrix(NA, nrow = nrow(cong), ncol = length(unique(cong$Plot)))
for (i in 1:nrow(cong)) {
  for (j in 1:length(unique(cong$Plot))) {
    Z[i, j] <- ifelse(sort(unique(cong$Plot))[j] == cong$Plot[i], 1, 0)
  }
}  


library(rjags)
####################################
#Fixed and random effects
####################################

data <- list(
  N = length(Y), # Number of observations        # Number of classes
  Dx = ncol(X),  #Number of columns of X
  Dz = ncol(Z), #Number of columns of Z
  Y = Y,     # Response variable
  X = X,      #Predictor matrix Fixed effects
  Z = Z     #predictor matrix Random effects
)


model_string <- "model {
  # Likelihood
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i]  <- inprod(X[i,], beta) + inprod(Z[i,], b)
  }
  
  # Random effects (group-level)
  for (j in 1:Dz) {
    b[j] ~ dnorm(0, tau2b)
  }
  
  # Fixed effects priors
  for (k in 1:Dx) {
    beta[k] ~ dnorm(0, 0.001)
  }
  

tau2b <- 1/(sigmab^2)
sigmab ~  dnorm(0, 0.001)T(0,10000)
tau <- 1/(sigma^2)
sigma ~  dnorm(0, 0.001)T(0,10000)  
}
"

setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels_inv.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels_inv.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

# ztest <- jags.samples(jags,c('beta','p','b',"sigma2b"),1000, thin = 1)
# str(ztest)

z <- jags.samples(jags,c('beta','b',"sigmab","sigma"),200000, thin = 200)
#z <- jags.samples(jags,c('beta','b',"sigma2b","sigma"),200000, thin = 200)

save(z, file = "/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_biomass_mg_20250929.RData")
load("/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_biomass_mg_20250929.RData")

plot(z$beta[2,,1], type = "l")
points(z$beta[2,,2], type = "l", col = "red")
points(z$beta[2,,3], type = "l", col = "blue")
acf(z$beta[2,,1])

hist(z$beta[2,,1:3]) #meadow vs submerged
hist(z$beta[3,,1:3]) #tyhyd vs submerged
hist(z$beta[4,,1:3]) #typha vs submerged

hist(z$beta[2,,1:3] - z$beta[3,,1:3]) #meadow vs tyhyd
hist(z$beta[2,,1:3] - z$beta[4,,1:3]) #meadow vs typha
hist(z$beta[3,,1:3] - z$beta[4,,1:3]) #tyhyd vs typha

dat_beta_post  <- rbind(data.frame(beta = c(z$beta[2,,1:3]), Treatment = "meadow", Baseline = "submerged"),
      data.frame(beta = c(z$beta[3,,1:3]), Treatment = "tyhyd", Baseline = "submerged"),
      data.frame(beta = c(z$beta[4,,1:3]), Treatment = "typha", Baseline = "submerged"),
      data.frame(beta = c(z$beta[2,,1:3] - z$beta[3,,1:3]), Treatment = "meadow", Baseline = "tyhyd"),
      data.frame(beta = c(z$beta[2,,1:3] - z$beta[4,,1:3]), Treatment = "meadow", Baseline = "typha"),
      data.frame(beta = c(z$beta[3,,1:3] - z$beta[4,,1:3]), Treatment = "tyhyd", Baseline = "typha"))
dat_beta_post$Treatment <- as.factor(dat_beta_post$Treatment)
dat_beta_post$Baseline <- as.factor(dat_beta_post$Baseline)
dat_beta_post$Baseline <- fct_relevel(dat_beta_post$Baseline, "tyhyd", "typha", "submerged")
ggplot(aes(x = beta),data = dat_beta_post) + facet_grid(Treatment ~ Baseline) + geom_density() + geom_vline(xintercept =  0, col = "red")


dat_beta_post %>% group_by(Treatment, Baseline) %>% 
  summarize(p_beta_gt0 = mean(beta>0), 
            post_mean_beta = mean(beta),
            beta_cred_lower90 = quantile(beta, 0.05),
            beta_cred_upper90 = quantile(beta, 0.95))


hist(z$sigmab)

####################################
#Inv_richness
####################################
Y <- cong  %>% select(Inv_richness) %>% pull(Inv_richness)
N <- nrow(Y)
#Y <- matrix(as.integer(Y),nrow = nrow(Y), ncol = ncol(Y))
#Creating the design matrix
#Meadow is baseline

################################################################################
#Submerged is a better choice for baseline!!!!
################################################################################
X <- cong %>% select(I_meadow, I_tyhyd, I_typha,DO_mg_L, Water_Temp_C,Veg_H, I_week)
#X <- cong %>% select(I_meadow,I_submerged, I_tyhyd, I_typha,DO_mg_L)
X <- data.frame(int = 1,as.matrix(X))
X <- as.matrix(X)



#Random effects desing matrix
Z <- matrix(NA, nrow = nrow(cong), ncol = length(unique(cong$Plot)))
for (i in 1:nrow(cong)) {
  for (j in 1:length(unique(cong$Plot))) {
    Z[i, j] <- ifelse(sort(unique(cong$Plot))[j] == cong$Plot[i], 1, 0)
  }
}  


library(rjags)
####################################
#Fixed and random effects
####################################

data <- list(
  N = length(Y), # Number of observations        # Number of classes
  Dx = ncol(X),  #Number of columns of X
  Dz = ncol(Z), #Number of columns of Z
  Y = Y,     # Response variable
  X = X,      #Predictor matrix Fixed effects
  Z = Z     #predictor matrix Random effects
)


model_string <- "model {
  # Likelihood
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i]  <- inprod(X[i,], beta) + inprod(Z[i,], b)
  }
  
  # Random effects (group-level)
  for (j in 1:Dz) {
    b[j] ~ dnorm(0, tau2b)
  }
  
  # Fixed effects priors
  for (k in 1:Dx) {
    beta[k] ~ dnorm(0, 0.001)
  }
  

tau2b <- 1/(sigmab^2)
sigmab ~  dnorm(0, 0.001)T(0,10000)
tau <- 1/(sigma^2)
sigma ~  dnorm(0, 0.001)T(0,10000)  
}
"

setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels_inv_rich.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels_inv_rich.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

# ztest <- jags.samples(jags,c('beta','p','b',"sigma2b"),1000, thin = 1)
# str(ztest)

z <- jags.samples(jags,c('beta','b',"sigmab","sigma"),200000, thin = 200)
#z <- jags.samples(jags,c('beta','b',"sigma2b","sigma"),200000, thin = 200)

save(z, file = "/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_Rich_20250929.RData")
load("/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_Rich_20250929.RData")

hist(z$sigmab)

plot(z$beta[2,,1], type = "l")
points(z$beta[2,,2], type = "l", col = "red")
points(z$beta[2,,3], type = "l", col = "blue")
acf(z$beta[2,,1])

hist(z$beta[2,,1:3]) #meadow vs submerged
hist(z$beta[3,,1:3]) #tyhyd vs submerged
hist(z$beta[4,,1:3]) #typha vs submerged

hist(z$beta[2,,1:3] - z$beta[3,,1:3]) #meadow vs tyhyd
hist(z$beta[2,,1:3] - z$beta[4,,1:3]) #meadow vs typha
hist(z$beta[3,,1:3] - z$beta[4,,1:3]) #tyhyd vs typha

dat_beta_post  <- rbind(data.frame(beta = c(z$beta[2,,1:3]), Treatment = "meadow", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[3,,1:3]), Treatment = "tyhyd", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[4,,1:3]), Treatment = "typha", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[2,,1:3] - z$beta[3,,1:3]), Treatment = "meadow", Baseline = "tyhyd"),
                        data.frame(beta = c(z$beta[2,,1:3] - z$beta[4,,1:3]), Treatment = "meadow", Baseline = "typha"),
                        data.frame(beta = c(z$beta[3,,1:3] - z$beta[4,,1:3]), Treatment = "tyhyd", Baseline = "typha"))
dat_beta_post$Treatment <- as.factor(dat_beta_post$Treatment)
dat_beta_post$Baseline <- as.factor(dat_beta_post$Baseline)
dat_beta_post$Baseline <- fct_relevel(dat_beta_post$Baseline, "tyhyd", "typha", "submerged")
ggplot(aes(x = beta),data = dat_beta_post) + facet_grid(Treatment ~ Baseline) + geom_density() + geom_vline(xintercept =  0, col = "red")

dat_beta_post %>% group_by(Treatment, Baseline) %>% 
  summarize(p_beta_gt0 = mean(beta>0), 
            post_mean_beta = mean(beta),
            beta_cred_lower90 = quantile(beta, 0.05),
            beta_cred_upper90 = quantile(beta, 0.95))

hist(z$sigmab)


####################################
#Inv_abun
####################################
Y <- cong  %>% select(Inv_abun) %>% pull(Inv_abun)
N <- nrow(Y)
#Y <- matrix(as.integer(Y),nrow = nrow(Y), ncol = ncol(Y))
#Creating the design matrix
#Meadow is baseline

################################################################################
#Submerged is a better choice for baseline!!!!
################################################################################
X <- cong %>% select(I_meadow, I_tyhyd, I_typha,DO_mg_L, Water_Temp_C,Veg_H, I_week)
#X <- cong %>% select(I_meadow,I_submerged, I_tyhyd, I_typha,DO_mg_L)
X <- data.frame(int = 1,as.matrix(X))
X <- as.matrix(X)



#Random effects desing matrix
Z <- matrix(NA, nrow = nrow(cong), ncol = length(unique(cong$Plot)))
for (i in 1:nrow(cong)) {
  for (j in 1:length(unique(cong$Plot))) {
    Z[i, j] <- ifelse(sort(unique(cong$Plot))[j] == cong$Plot[i], 1, 0)
  }
}  


library(rjags)
####################################
#Fixed and random effects
####################################

data <- list(
  N = length(Y), # Number of observations        # Number of classes
  Dx = ncol(X),  #Number of columns of X
  Dz = ncol(Z), #Number of columns of Z
  Y = Y,     # Response variable
  X = X,      #Predictor matrix Fixed effects
  Z = Z     #predictor matrix Random effects
)


model_string <- "model {
  # Likelihood
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i]  <- inprod(X[i,], beta) + inprod(Z[i,], b)
  }
  
  # Random effects (group-level)
  for (j in 1:Dz) {
    b[j] ~ dnorm(0, tau2b)
  }
  
  # Fixed effects priors
  for (k in 1:Dx) {
    beta[k] ~ dnorm(0, 0.001)
  }
  

tau2b <- 1/(sigmab^2)
sigmab ~  dnorm(0, 0.001)T(0,10000)
tau <- 1/(sigma^2)
sigma ~  dnorm(0, 0.001)T(0,10000)  
}
"

setwd("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/")
write(model_string,"bug_hotels_inv_abun.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/Dropbox/CDSC/bug_hotels/bug_hotels_inv_abun.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

# ztest <- jags.samples(jags,c('beta','p','b',"sigmab"),1000, thin = 1)
# str(ztest)

z <- jags.samples(jags,c('beta','b',"sigmab","sigma"),200000, thin = 200)
#z <- jags.samples(jags,c('beta','b',"sigma2b","sigma"),200000, thin = 200)

save(z, file = "/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_Abun_20250929.RData")
load("/Users/gregorymatthews/Dropbox/bug_hotels/z_Inv_Abun_20250929.RData")

hist(z$sigmab)

plot(z$beta[2,,1], type = "l")
points(z$beta[2,,2], type = "l", col = "red")
points(z$beta[2,,3], type = "l", col = "blue")
acf(z$beta[2,,1])

hist(z$beta[2,,1:3]) #meadow vs submerged
hist(z$beta[3,,1:3]) #tyhyd vs submerged
hist(z$beta[4,,1:3]) #typha vs submerged

hist(z$beta[2,,1:3] - z$beta[3,,1:3]) #meadow vs tyhyd
hist(z$beta[2,,1:3] - z$beta[4,,1:3]) #meadow vs typha
hist(z$beta[3,,1:3] - z$beta[4,,1:3]) #tyhyd vs typha

dat_beta_post  <- rbind(data.frame(beta = c(z$beta[2,,1:3]), Treatment = "meadow", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[3,,1:3]), Treatment = "tyhyd", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[4,,1:3]), Treatment = "typha", Baseline = "submerged"),
                        data.frame(beta = c(z$beta[2,,1:3] - z$beta[3,,1:3]), Treatment = "meadow", Baseline = "tyhyd"),
                        data.frame(beta = c(z$beta[2,,1:3] - z$beta[4,,1:3]), Treatment = "meadow", Baseline = "typha"),
                        data.frame(beta = c(z$beta[3,,1:3] - z$beta[4,,1:3]), Treatment = "tyhyd", Baseline = "typha"))
dat_beta_post$Treatment <- as.factor(dat_beta_post$Treatment)
dat_beta_post$Baseline <- as.factor(dat_beta_post$Baseline)
dat_beta_post$Baseline <- fct_relevel(dat_beta_post$Baseline, "tyhyd", "typha", "submerged")
ggplot(aes(x = beta),data = dat_beta_post) + facet_grid(Treatment ~ Baseline) + geom_density() + geom_vline(xintercept =  0, col = "red")

dat_beta_post %>% group_by(Treatment, Baseline) %>% 
  summarize(p_beta_gt0 = mean(beta>0), 
            post_mean_beta = mean(beta),
            beta_cred_lower90 = quantile(beta, 0.05),
            beta_cred_upper90 = quantile(beta, 0.95))

hist(z$sigmab)

