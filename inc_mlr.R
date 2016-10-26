setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")

inc<-read.csv("incubation.csv")
View(inc)

mlr<-lm(inc$avg_diff13C~inc$avg_ch4+inc$avg_co2)
summary(mlr)
resid(mlr)
shapiro.test(resid(mlr)) #normally distributed model residuals

mlr$coefficients

preds<-c(predict(mlr))
preds

plot(inc$avg_diff13C,preds)

