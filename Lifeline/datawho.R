setwd('C:/Users/Admin/Desktop/UM FILE/Semester 4/Intro Data Science/R/Lifeline/')
cty<-read.csv(file='clean data/asean.csv')
me<-read.csv(file='clean data/asean_meow.csv')
bye<-read.csv(file='clean data/asean_bye.csv')
file4<-read.csv(file='clean data/asean_4.csv')
head(cty)

tail(cty)
head(file4)
install.packages('rhandsontable')
library(rhandsontable)