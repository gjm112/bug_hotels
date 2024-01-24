dat <- read.csv("./data/Logan_Invertebrate_Munuscong_Data.csv")

#All data from Munuscong
table(dat$Site)

#Four different plots with 8 repolicates each
table(dat$Plot)

#Four different treatments 
table(dat$Treatment)
