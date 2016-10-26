setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

anoxic<-read.csv('anoxic.csv')
View(anoxic)

hist(anoxic$co2_prod)
shapiro.test(anoxic$co2_prod) #yes, normal

xlab_inc_co2=expression(paste(CO[2] ~ "Production Rate" ~ (mu*mol ~ day^{-1})))
ylab=expression(Delta, " ", delta^{13}*"C")

quartz()
par(mar=c(5,5,2,1))
plot(anoxic$co2_prod,anoxic$avg_diff13c,ylab=ylab,xlab=xlab_inc_co2,pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5,ylim=c(-2.08,-0.4),xlim=c(-30,243))
arrows(anoxic$co2_prod,anoxic$avg_diff13c+anoxic$se_diff13c,anoxic$co2_prod,anoxic$avg_diff13c-anoxic$se_diff13c,length=0.05, angle = 90,code=3,lwd=1.5)
arrows(anoxic$co2_prod+anoxic$se_co2,anoxic$avg_diff13c,anoxic$co2_prod-anoxic$se_co2,anoxic$avg_diff13c,length=0.05,angle=90,code=3,lwd=1.5)
mod<-lm(anoxic$avg_diff13c~anoxic$co2_prod)
summary(mod)
abline(mod)
cor.test(anoxic$co2_prod,anoxic$avg_diff13c)
shapiro.test(resid(mod)) #yes, normally distributed residuals
