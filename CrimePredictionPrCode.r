install.packages("haven")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("MASS")
install.packages("corrplot")
library("corrplot")

library(ggplot2)
library(haven)
library(MASS)
library(haven)

dataframe <-read_sas("C:/Users/User/Desktop/Биостатистика/Проект/crime.sas7bdat")
View(dataframe)
colnames(dataframe) 
summary(dataframe)

sapply(dataframe, class)


ggplot(dataframe, aes( x =  state , y=  crime )) +geom_bar(stat="identity")

dataframe2 = subset(dataframe, select = -c(1,2)) ##mahame state
dataframe2

cor_table <- cor(dataframe2)
cor_table
corrplot(cor_table,method = "square")
corrplot(cor_table,method = "number")

# Poisson regression Model
summary(m1 <-glm(crime~pctmetro +pctwhite +pcths+murder+
                   poverty +single-1 , family="poisson"(link="log") , data = dataframe))

pchisq(m1$deviance, m1$df.residual, lower.tail=F)
E1 <-resid(m1, type="pearson")
F1 <-fitted(m1, type="response")


p1 <-scatter.smooth(F1, E1, cex.lab =1.5, xlab="Fitted values", ylab="Pearson Residuals")
abline(h =0, v=0, lty=2); text(F1, E1, labels =row.names(dataframe$state), pos =4)






# negative binomial regression model 
summary(m3 <-glm.nb(crime~pctmetro +pctwhite +pcths+murder+
                      poverty +single-1 , data = dataframe))
pchisq(m3$deviance, m3$df.residual, lower.tail=F)


E3 <-resid(m3, type="pearson")
F3 <-fitted(m3, type="response")

p3<-scatter.smooth(F3, E3, cex.lab =1.5, xlab="Fitted values", ylab="Pearson Residuals")


abline(h =0, v=0, lty=2); text(F1, E1,row.names(dataframe$state), pos =4)

prediction_crime<-predict.glm(m3,type ="response")


round(prediction_crime,digits=0)
ggplot(dataframe, aes( x =  state , y=prediction_crime )) +geom_bar(stat="identity")
