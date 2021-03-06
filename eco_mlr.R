setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

eco<-read.csv('ecosystem.csv')
decomp<-read.csv('decomposition.csv')

View(eco)
View(decomp)

####CH4 + Loss in Tensile Strength (Decomp)####
mlr_eco<-lm(eco$avg_diff13C~eco$log_ch4+decomp$avg_loss_DD)
summary(mlr_eco)
resid(mlr_eco)
shapiro.test(resid(mlr_eco))

AIC(mlr_eco) #9.021501
mlr_eco$coefficients
eco_preds<-c(predict(mlr_eco))

quartz()
par(mar=c(5,5,2,1))
plot(eco_preds,eco$avg_diff13C,pch=16,xlab=NA,ylab=NA,cex=1.5,cex.lab=1.5,cex.axis=1.5,ylim=c(-2.0,0),xlim=c(-2.0,0)) #x = modeled, y = observed
abline(0,1,lty=5,lwd=2)
mod<-lm(eco$avg_diff13C~eco_preds)
summary(mod)

####CH4 only####
methane_eco<-lm(eco$avg_diff13C~eco$log_ch4)
summary(methane_eco)
resid(methane_eco)
shapiro.test(resid(methane_eco))
AIC(methane_eco) #7.08269

####decomp only####
decomp<-lm(eco$avg_diff13C~decomp$avg_loss_DD)
summary(decomp)
resid(decomp)
shapiro.test(resid(decomp))
AIC(decomp)
