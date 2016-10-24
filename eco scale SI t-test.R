setwd("~/Desktop/Alaska Project/JH Stats Analysis/Bidirectional Plots/")
eco<-read.csv('eco_ttest.csv')

t.test(eco$before,eco$after,paired=T)
