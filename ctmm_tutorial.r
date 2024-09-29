library(ctmm)
library(tidyr)
library(sf)
library(sp)

#ARTIFICIAL DATA EXAMPLE
########################

#set movement model
model=ctmm(tau=1,sigma=1,mu=c(0,0))

#get artificial data
sim = simulate(model,t=seq(1, 1000, by=1))
sim = ctmm:::pseudonymize(sim)

#plot trajectory
plot(sim)

#get estimation of home-range (expected area to be used by the animal)
guess = ctmm.guess(sim)
fit = ctmm.fit(sim,guess) #!!!!!!
UD = akde(sim,fit)

#plot home-range
plot(UD)

#additional information extrected by ctmm
summary(fit)

#DATA EXAMPLE
###############################

#get data from csv
path1= "C:/Users/nadeem56/Desktop/INadeem/Mongolian gazelle.csv"
ind = read.csv(path1)
telemetry = as.telemetry(ind)

#plot trajectory
plot(telemetry[[4]],error=FALSE)# for 1 guy- list 

#get estimation of home-range (expected area to be used by the animal)
guess = ctmm.guess(telemetry[[4]])
fit = ctmm.fit(telemetry[[4]],guess)
UD = akde(telemetry[[4]],fit)

#plot home-range
plot(UD,errors=FALSE)
#additional information extrected by ctmm
summary(fit)