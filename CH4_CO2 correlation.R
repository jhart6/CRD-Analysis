#CH4 - CO2 correlation in 2014 incubations?

setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")
inc<-read.csv("incubation.csv")

quartz()
par(mar=c(5,5,2,1))
plot(inc$avg_ch4,inc$avg_co2,pch=16)
cor.test(inc$avg_ch4,inc$avg_co2)
mod<-lm(inc$avg_co2~inc$avg_ch4)
summary(mod)
abline(mod)

decomp<-read.csv("decomposition.csv")
quartz()
par(mar=c(5,5,2,1)
plot(decomp$avg_loss_DD,inc$avg_co2,pch=16)
cor.test(decomp$avg_loss_DD,inc$avg_co2)
mod<-lm(inc$avg_co2~decomp$avg_loss_DD)
summary(mod)
abline(mod)
