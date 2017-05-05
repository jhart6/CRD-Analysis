#sediment PW and incubation isotope analysis
#more oxidation with more PW sulfate?

#first cut with pond level data
setwd('~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/')
pw<-read.csv("isotope_pw_analysis.csv")

plot(pw$avg_acetate,pw$avg_diff13C)
acetate<-lm(pw$avg_diff13C~pw$avg_acetate)
summary(acetate)
abline(acetate)
shapiro.test(resid(acetate))
AIC(acetate) #22.04

plot(pw$avg_nitrate,pw$avg_diff13C)
nitrate<-lm(pw$avg_diff13C~pw$avg_nitrate)
summary(nitrate)
abline(nitrate)
shapiro.test(resid(nitrate))
AIC(nitrate) #22.12

plot(pw$avg_sulfate,pw$avg_diff13C)
sulfate<-lm(pw$avg_diff13C~pw$avg_sulfate)
summary(sulfate)
abline(sulfate)
shapiro.test(resid(sulfate))
AIC(sulfate) #22.49

acetate.sulfate<-lm(pw$avg_diff13C~pw$avg_acetate+pw$avg_sulfate)
summary(acetate.sulfate)
shapiro.test(resid(acetate.sulfate))
AIC(acetate.sulfate) #23.98


####second cut with bottle level data####
bottle<-read.csv('all bottle level data.csv')
shapiro.test(bottle$Acetate.umol.ml) #not normal
shapiro.test(bottle$Sulfate.umol.ml)#not normal
shapiro.test(bottle$Nitrate.umol.ml)#not normal
shapiro.test(bottle$CH4.umol.day)#not normal
shapiro.test(bottle$delta.del13C) #normal
#log transformation does not help these variables


#predicting oxic isotopes
plot(bottle$Sulfate.umol.ml[which(bottle$Treatment=='oxic')],bottle$delta.del13C[which(bottle$Treatment=='oxic')],ylab='Delta del13C',xlab='PW Sulfate (umol/ml)',pch=16,main='Oxic')
mod<-lm(bottle$delta.del13C[which(bottle$Treatment=='oxic')]~bottle$Sulfate.umol.ml[which(bottle$Treatment=='oxic')])
summary(mod)
abline(mod)
AIC(mod) #66.33127

oxic<-lm(bottle$delta.del13C[which(bottle$Treatment=="oxic")]~bottle$CH4.umol.day[which(bottle$Treatment=='oxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="oxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="oxic")]+bottle$Nitrate.umol.ml[which(bottle$Treatment=="oxic")])
summary(oxic)
AIC(oxic) #69.03119

oxic.with.pond<-lm(bottle$delta.del13C[which(bottle$Treatment=="oxic")]~bottle$Pond[which(bottle$Treatment=='oxic')]+bottle$CH4.umol.day[which(bottle$Treatment=='oxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="oxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="oxic")]+bottle$Nitrate.umol.ml[which(bottle$Treatment=="oxic")])
summary(oxic.with.pond)
AIC(oxic.with.pond) #70.3774

oxic.no.nitrate<-lm(bottle$delta.del13C[which(bottle$Treatment=="oxic")]~bottle$CH4.umol.day[which(bottle$Treatment=='oxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="oxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="oxic")])
summary(oxic.no.nitrate)
AIC(oxic.no.nitrate) #69.26647

acetate.sulfate<-lm(bottle$delta.del13C[which(bottle$Treatment=='oxic')]~bottle$Acetate.umol.ml[which(bottle$Treatment=='oxic')]+bottle$Sulfate.umol.ml[which(bottle$Treatment=='oxic')])
summary(acetate.sulfate)
AIC(acetate.sulfate) #67.61213


#predicting anoxic isotopes
plot(bottle$Sulfate.umol.ml[which(bottle$Treatment=='anoxic')],bottle$delta.del13C[which(bottle$Treatment=='anoxic')],ylab='Delta del13C',xlab='PW Sulfate (umol/ml)',pch=16,main='Anoxic')
mod2<-lm(bottle$delta.del13C[which(bottle$Treatment=='anoxic')]~bottle$Sulfate.umol.ml[which(bottle$Treatment=='anoxic')])
summary(mod2)
abline(mod2)
AIC(mod2) #37.59466

anoxic<-lm(bottle$delta.del13C[which(bottle$Treatment=="anoxic")]~bottle$CH4.umol.day[which(bottle$Treatment=='anoxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="anoxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="anoxic")]+bottle$Nitrate.umol.ml[which(bottle$Treatment=="anoxic")])
summary(anoxic)
AIC(anoxic) #36.91745

anoxic.with.pond<-lm(bottle$delta.del13C[which(bottle$Treatment=="anoxic")]~bottle$Pond[which(bottle$Treatment=='anoxic')]+bottle$CH4.umol.day[which(bottle$Treatment=='anoxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="anoxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="anoxic")]+bottle$Nitrate.umol.ml[which(bottle$Treatment=="anoxic")])
summary(anoxic.with.pond)
AIC(anoxic.with.pond) #31.23594

anoxic.no.nitrate<-lm(bottle$delta.del13C[which(bottle$Treatment=="anoxic")]~bottle$CH4.umol.day[which(bottle$Treatment=='anoxic')]+bottle$Acetate.umol.ml[which(bottle$Treatment=="anoxic")]+bottle$Sulfate.umol.ml[which(bottle$Treatment=="anoxic")])
summary(anoxic.no.nitrate)
AIC(anoxic.no.nitrate) #36.69

anoxic.acetate.sulfate<-lm(bottle$delta.del13C[which(bottle$Treatment=='anoxic')]~bottle$Acetate.umol.ml[which(bottle$Treatment=='anoxic')]+bottle$Sulfate.umol.ml[which(bottle$Treatment=='anoxic')])
summary(anoxic.acetate.sulfate)
AIC(anoxic.acetate.sulfate) #39.59244
