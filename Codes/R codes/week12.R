#w12/w13/presentation/report
library(readr)
library(fastDummies)
library(huxtable)
library(jtools)
dataf <- read_csv("regressiondata3nov.csv")
dataf2 <- dummy_cols(dataf, select_columns = c('Major','Academic.Interest.1','Academic.Interest.2'))
a <- dataf2[,c(8,13:33)]
reg <- glm(IndGrd2 ~ ., data = a, family = "gaussian")
summary(reg)

b <- dataf2[,c(8,34:49)]
reg2 <- glm(IndGrd2 ~ ., data = b, family = "gaussian")
summary(reg2)

c <- dataf2[,c(8,50:65)]
reg3 <- glm(IndGrd2 ~ ., data = c, family = "gaussian")
summary(reg3)


allreg <- glm(IndGrd2 ~ ., data = c(a,b,c), family = "gaussian")
summary(allreg)

without <- glm(IndGrd2 ~ Major, data=dataf, family = "gaussian")
summary(without)

#leave out the declared majors.
#to what extent does your qr grade decide ur major.
#stick to academic interests for now.

#pool the info from academic info
#create 15 new cols for majors
#for each entry, we comnbine if major is in ac1 OR ac2
#and then we use them for Regression.
#Check the gender and region

summ(reg2)
summ(reg3)

df2 <- dummy_cols(dataf, select_columns = c('Sex.x','Region'))
str(df2)
Sexx <-  glm(IndGrd2 ~ Sex.x_F + Sex.x_M, data = df2, family = "gaussian")
summ(Sexx)
ugh <- df2[,c(8,16:23)]
reg <- glm(IndGrd2 ~ ., data = ugh, family = "gaussian")
summ(reg)
summary(reg)

