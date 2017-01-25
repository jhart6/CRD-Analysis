setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

inc<-read.csv("incubation.csv")
View(inc)

cor.test(inc$avg_ch4,inc$avg_co2)
plot(inc$avg_ch4,inc$avg_co2)
mod<-lm(inc$avg_co2~inc$avg_ch4)
abline(mod)


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
plot(preds,inc$avg_diff13C,pch=16,xlab=NA,ylab=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,ylim=c(-2.0,0),xlim=c(-2.0,0)) #x = modeled, y = observed
abline(0,1,lty=5,lwd=2)
mod<-lm(inc$avg_diff13C~preds)
summary(mod)
