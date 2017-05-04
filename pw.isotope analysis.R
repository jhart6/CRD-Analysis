#sediment PW and incubation isotope analysis
#more oxidation with more PW sulfate?

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


