#Script for Bidirectional plots for CRD Manuscript 
#5/16/16

setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots")

inc<-read.csv("incubation.csv")
eco<-read.csv('ecosystem.csv')
decomp<-read.csv('decomposition.csv')

xlab_inc=expression(paste(CH[4] ~ "Production Rate" ~ (mu*mol ~ day^{-1})))
xlab_inc_co2=expression(paste(CO[2] ~ "Production Rate" ~ (mu*mol ~ day^{-1})))
xlab_eco=expression(CH[4] ~ "Production Rate" ~ (mu*mol ~ m^{-2} ~ day^{-1}))
xlab_decomp=expression("% Tensile Loss Degree" ~ Day^{-1})
ylab=expression(Delta, " ", delta^{13}*"C")

#####Incubation Scale Plot#####
quartz()
par(mar=c(5,5,2,1))
plot(inc$log_ch4,inc$avg_diff13C,ylim=range(c(inc$avg_diff13C+inc$se_diff13C,inc$avg_diff13C-inc$se_diff13C)),xlim=range(c(inc$log_ch4-inc$se_log_ch4,inc$log_ch4+inc$se_log_ch4)),ylab=NA,xlab=NA,pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5,xaxt='n',yaxt='n')
arrows(inc$log_ch4,inc$avg_diff13C+inc$se_diff13C,inc$log_ch4,inc$avg_diff13C-inc$se_diff13C,length=0.05, angle = 90,code=3,lwd=1.5)
arrows(inc$log_ch4+inc$se_log_ch4,inc$avg_diff13C,inc$log_ch4-inc$se_log_ch4,inc$avg_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
mod<-lm(inc$avg_diff13C~inc$log_ch4)
abline(mod)
summary(mod)
x_axis=c(-2,-1,0,1,2)
y_axis=c(-2.5, -2.0, -1.5, -1.0, -0.5, 0)
Axis(side=1,at=x_axis,labels=FALSE)
Axis(side=2,at=y_axis,labels=FALSE)
cor.test(inc$log_ch4,inc$avg_diff13C,method='pearson')

#non-logged alternative
quartz()
par(mar=c(5,5,2,1))
plot(inc$avg_ch4,inc$avg_diff13C,ylim=range(c(inc$avg_diff13C+inc$se_diff13C,inc$avg_diff13C-inc$se_diff13C)),xlim=range(c(inc$avg_ch4-inc$se_ch4,inc$avg_ch4+inc$se_ch4)),ylab=NA,xlab=NA,pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5)
arrows(inc$avg_ch4,inc$avg_diff13C+inc$se_diff13C,inc$avg_ch4,inc$avg_diff13C-inc$se_diff13C,length=0.05, angle = 90,code=3,lwd=1.5)
arrows(inc$avg_ch4+inc$se_ch4,inc$avg_diff13C,inc$avg_ch4-inc$se_ch4,inc$avg_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
mod<-lm(inc$avg_diff13C~inc$avg_ch4)
summary(mod)
abline(mod)
cor.test(inc$avg_ch4,inc$avg_diff13C)

#all incubation data - no pond averages
inc_all<-read.csv("inc_all data.csv")
plot(inc_all$ch4_prod,inc_all$diff13C)
mod<-lm(inc_all$diff13C~inc_all$ch4_prod)
abline(mod)
cor.test(inc_all$ch4_prod,inc_all$diff13C,method='pearson')


#paired t-test (are before and after SI different?)
t.test(inc_all$before,inc_all$after,paired=T)

##CO2 Bidirectional Plot##
quartz()
par(mar=c(5,5,2,1))
plot(inc$avg_co2,inc$avg_diff13C,ylim=range(c(inc$avg_diff13C+inc$se_diff13C,inc$avg_diff13C-inc$se_diff13C)),xlim=range(c(inc$avg_co2-inc$se_co2,inc$avg_co2+inc$se_co2)),ylab=NA,xlab=NA,pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5)
arrows(inc$avg_co2,inc$avg_diff13C+inc$se_diff13C,inc$avg_co2,inc$avg_diff13C-inc$se_diff13C,length=0.05, angle = 90,code=3,lwd=1.5)
arrows(inc$avg_co2+inc$se_co2,inc$avg_diff13C,inc$avg_co2-inc$se_co2,inc$avg_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
modCO2<-lm(inc$avg_diff13C~inc$avg_co2)
abline(modCO2)


####Ecosystem Scale Plots####
###CH4 Plot###
quartz()
par(mar=c(5,5,2,1))
plot(eco$log_ch4,eco$avg_diff13C,ylab=NA,xlab=NA,xlim=range(c(eco$log_ch4-eco$se_log_ch4,eco$log_ch4+eco$se_log_ch4)),ylim=range(c(-2,0.17)),pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5)
arrows(eco$log_ch4,eco$avg_diff13C+eco$se_diff13C,eco$log_ch4,eco$avg_diff13C-eco$se_diff13C,length=0.05, angle = 90,code=3,lwd=1.5)
arrows(eco$log_ch4+eco$se_log_ch4,eco$avg_diff13C,eco$log_ch4-eco$se_log_ch4,eco$avg_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
mod1<-lm(eco$avg_diff13C~eco$log_ch4)
abline(mod1)
summary(mod1)
cor.test(eco$log_ch4,eco$avg_diff13C,method='pearson')

###DecompT Plot###
#Decomp = % Loss per DD (calculated new way using only year long changes)
quartz()
par(mar=c(5,5,2,1))
plot(decomp$avg_loss_DD,decomp$avg_diff13C,xlab=NA,ylab=NA,xlim=range(c(decomp$avg_loss_DD-decomp$se_loss_DD,decomp$avg_loss_DD+decomp$se_loss_DD)),ylim=range(c(decomp$avg_diff13C-decomp$se_diff13C,decomp$avg_diff13C+decomp$se_diff13C)),pch=16,cex=1.5,cex.lab=1.5,cex.axis=1.5)
arrows(decomp$avg_loss_DD,decomp$avg_diff13C+decomp$se_diff13C,decomp$avg_loss_DD,decomp$avg_diff13C-decomp$se_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
arrows(decomp$avg_loss_DD-decomp$se_loss_DD,decomp$avg_diff13C,decomp$avg_loss_DD+decomp$se_loss_DD,decomp$avg_diff13C,length=0.05,angle=90,code=3,lwd=1.5)
mod2<-lm(decomp$avg_diff13C~decomp$avg_loss_DD)
abline(mod2)
summary(mod2)
cor.test(decomp$avg_loss_DD,decomp$avg_diff13C,method='pearson')

hist(resid())