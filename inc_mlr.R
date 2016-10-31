setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

inc<-read.csv("incubation.csv")
View(inc)

####CH4 + CO2####
mlr<-lm(inc$avg_diff13C~inc$avg_ch4+inc$avg_co2)
summary(mlr)
resid(mlr)
shapiro.test(resid(mlr)) #normally distributed model residuals

mlr$coefficients
preds<-c(predict(mlr))

####CO2 + CH4####
mlr<-lm(inc$avg_diff13C~inc$avg_co2+inc$avg_ch4)
summary(mlr)
resid(mlr)
shapiro.test(resid(mlr)) #normally distributed model residuals

mlr$coefficients
preds<-c(predict(mlr))


quartz()
par(mar=c(5,5,2,1))
plot(preds,inc$avg_diff13C,pch=16,cex=1.5,xlab=expression("Observed"),ylab=expression("Modeled"))
abline(0,1,col='red')
mod<-lm(inc$avg_diff13C~preds)
summary(mod)
