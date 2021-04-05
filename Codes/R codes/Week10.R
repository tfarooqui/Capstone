#Week 10 Capstone Meeting (Fri, 23rd Oct)

library(mice)
library(readr)
library(VIM)
library(lattice)
library(ggplot2)
library(dplyr) 

dat <- read_csv("QR2regressiondata.csv")
sapply(dat, function(x) sum(is.na(x))) #Missing values.
colnames(dat)[11:11] <- c('SAT','HSG') #Changing names of really long column names...

#Next step is to transform the variables in factors or numeric. 
#For example, High School Grades are categorical variables, whereas SAT/QR Final Grades are continuous.


dat2 <-dat %>%
  mutate(
    Sex.x = as.factor(Sex.x),
    Region = as.factor(Region),
    HSG = as.factor(HSG),
    Year= as.factor(Year),
    SAT = as.numeric(SAT),
    Grade = as.numeric(Grade)
  )
#Look the dataset structure.
str(dat2)
mylogit <- glm(Grade ~ SAT + HSG, data = dat2, family = "gaussian")
summary(mylogit) 

mylogit2 <- glm(Grade ~ SAT + HSG + Sex.x + Region + Year, data = dat2, family = "gaussian")
summary(mylogit2)

#Since we have lots of missing data, listwise deletion and mean/median substitution won't work.
#Instead we use Multiple Imputation. With this approach, rather than replacing missing values with a single value, 
#we use the distribution of the observed data/variables to estimate multiple possible values for the data points. 
#This allows us to account for the uncertainty around the true value, and obtain approximately unbiased estimates.
#Also allows more flexibility in deciding how many imputations are necessary.


#How does MICE (Multivariate Imputation by Chained Equations) work?

# a) choose values that keep the relationship in the dataset intact in place of missing values 
# b) create independently drawn imputed (usually 5) datasets 
# c) calculate new standard errors using variation across datasets to take into account the
# uncertainty created by these imputed datasets 

#Time to impute data using MICE, and see if the results change.

#We run the mice code with 0 iterations 
imp <- mice(dat2, maxit=0)
#PredictorMatrix: 1 means a variable is used to predict another variable, 0 otherwise
predM = imp$predictorMatrix
head(predM)

meth = imp$method
#Specify a separate imputation model for variables of interest,
#and Turn their methods matrix into the specified imputation models

#Ordered categorical variables - High School Grades
poly <- c("HSG")
meth[poly] = "polr"

# Dichotomous variable
log <- c("Sex.x")
meth[log] = "logreg"

#Unordered categorical variable 
#poly2 <- c("")
#meth[poly2] = "polyreg"

satt <- c("SAT")
meth[satt] = "norm.nob"

meth # our variables of interest are now configured to be imputed with the imputation method we specified
#Empty = no imputation

#Now that we are ready for multiple imputation
#With this command, we tell mice to impute the QR data, create 5 datasets, use predM as the predictor matrix
#print the imputationprocess
imp2 <- mice(dat2, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  TRUE)
#We now have 5 imputed datasets. Across all datasets, non-missing values are the same. 
#The imputation created 5 datasets with different plausible values for missing values
fitm <- with(imp2, lm(Grade ~ HSG + SAT))
summary(fitm)
summary(pool(fitm))


#make grades ordinal
#Do more research on mice
#why chose certian methods over others.
#Go back and seperate individual components of the grades.
#Irats- 10%, Peer eval- 15%, Instructor eval 10%, Writing assignment 10%, Exam 1/2 15%
#Reweigh all of them
#Make sure they are all out of 100
#take average to get a Scaled grade.

#Incorporate the data from 14/15.

#wgat is the prob
# what have we done so far
#What are we gonna do next sem.






# init = mice(dat2, maxit=0) 
# meth = init$method
# predM = init$predictorMatrix
# imputed = mice(dat2, method=meth, predictorMatrix=predM, m=5)
# imputed <- complete(imputed)
# sapply(imputed, function(x) sum(is.na(x)))
# #High School Grades
# original <- dat2
# actual <- original$HSG[is.na(dat2$HSG)]
# predicted <- imputed$HSG[is.na(dat2$HSG)]
# #SAT
# actual <- original$Translated.SAT.I.Math[is.na(dat2$Translated.SAT.I.Math)] 
# predicted <- imputed$Translated.SAT.I.Math[is.na(dat2$Translated.SAT.I.Math)] 
# table(actual)
# table(predicted)
# mean(actual)
# mean(predicted)

#Sources: https://uvastatlab.github.io/2019/05/01/getting-started-with-multiple-imputation-in-r/
