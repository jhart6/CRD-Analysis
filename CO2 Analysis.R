setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

inc_all<-read.csv("inc_all data.csv")
View(inc_all)

hist(inc_all$co2_prod)
shapiro.test(inc_all$co2_prod) #normal

hist(inc_all$diff13C)
shapiro.test(inc_all$dif) #normal

hist(inc_all$ch4_prod)
shapiro.test(inc_all$ch4_prod) #not normal, but close
y=log(inc_all$ch4_prod)
hist(y)
shapiro.test(y)

quartz()
plot(inc_all$co2_prod,inc_all$diff13C)
mod<-lm(inc_all$diff13C~inc_all$co2_prod)
summary(mod) #significant, negative
abline(mod)


quartz()
plot(inc_all$ch4_prod,inc_all$diff13C)
