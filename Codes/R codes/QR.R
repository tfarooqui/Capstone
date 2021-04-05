library(readr)
TalhaQRdata <- read_csv("TalhaQRdata.csv")
View(TalhaQRdata)

QR_1618 <- TalhaQRdata[335:1012,]
write.csv(QR_1618," QR1618.csv", row.names = TRUE)

boxplot(QR_1618$Grade,main = "Box-Plot of Final QR Grades",
        ylab = "Marks",
        col = "orange",
        border = "black")
boxplot(QR_1618$Exam1, main = "Box-Plot of QR mid-term 1 Grades",
        ylab = "Marks in mid-term 1",
        col = "orange",
        border = "black")
boxplot(QR_1618$Exam2, main = "Box-Plot of QR mid-term 2 Grades",
        ylab = "Marks in mid-term 2",
        col = "orange",
        border = "black")

plot(QR_1618$Grade)

TalhaQRADM <- read_csv("TalhaQRADM.csv")
QRADM_1618 <- TalhaQRADM[1:690,]
write.csv(QRADM_1618," QRADM1618.csv", row.names = TRUE)
boxplot(QRADM_1618$Translated.SAT.I.Math)
boxplot(QRADM_1618$ACT.Math.Verified)

which(QR_1618$PIN=="N385115424262")




#fit log reg model
#how to deal with lots of missing values
#thinking about what data we have and my questions
#are there questions i wanna ask?
#additional data??
#methods

QR_1618[QR_1618=="1"]<-0
#QR_1618[QR_1618=="F"]<-0
QR_1618[QR_1618=="M"]<-1

QR_1618[QR_1618 >= 75] <-1
m <- QR_1618$Grade
m[m >=75] <- 1
m[m >1] <- 0

xtabs(~Grade2 + Sex, data = QR_1618)
QR_1618$Sex <- factor(QR_1618$Sex)

QR_1618$Grade2=m

logreg <- glm(Grade2 ~ Sex + Exam1 + Exam2 , data=QR_1618, family = binomial(link = "logit"))
summary(logreg)

confint(logreg)
confint.default(logreg)

exp(coef(logreg))
exp(cbind(OR = coef(logreg), confint(logreg)))

#reweigh categories
# make new variable (academic performance varaible - want to predict)
# predictors: sex,region, intended major
# dummy variable: 1 variable with 3 levels: turn to 2 varibale with 3 levels
# dont include all 3
# leave one dummy variable out.
# divide the majors into smaller categories/groups (4/5)
# 
# 2 columns for majors: math 1st/2nd choice
# 28 DV instead of 14
# 
# how do i incorporate the tests: 
#         
# get some info from those categories (sat,act)
# way to combine into single score
# distribution of sat scores and compare to a levels
# 
# subset data for each oneway.test(eg sat score
#  do rregression + sat score wit quantittive)
# 
# put average
# keep track where the NAs were.
# 
# package vignettes
#r package_name vignette


library(Amelia)
library(mlbench)
missmap(QR_1618, col=c("white", "black"), legend=FALSE)
missmap(QRADM_1618, col=c("red", "black"), legend=FALSE)
#sum((is.na(QRADM_1618$SAT.I.Math))/690) Percentage of missing SAT Values.
#impude data using package mice.
#Or we go full bayesian. Pred dist and 
#eliminate students with missing values. Remove rows. 
#inspired by Item response theory.
#variable selection
#Bayesian variable selection
#create model for each covariate.
#See distributions of ACT/SAT. combine test scores into single variable.
#smash H2 math/pjhysics together. (Rescale with IB.)
#Match high school scores from diff systems (IB/Alevels/AP scores).
#Try on Singaporean males. 
#Gender and Ethniticity.
#Use as.factor in R
#Predict the log row scores.
#High school is ordinal.
#Start clustering students ability wise.
#unique(QR_1618$Region)


library(corrplot)
correlations <- cor(QR_1618[,6:16])
corrplot(correlations, method="circle")

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- (QR_1618[,6:16])


library(tidyr)


delete.na <- function(DF, n=0) {
        DF[rowSums(is.na(DF)) <= n,]
}
delete.na(QRADM_1618)

mean(QRADM_1618$SAT.I.Math, na.rm = TRUE)
mean(QRADM_1618$Translated.SAT.I.Math, na.rm = TRUE)
mean(QRADM_1618$SAT.R.Math.Verified, na.rm = TRUE)
mean(QRADM_1618$ACT.Math.Verified, na.rm = TRUE)

#vars_to_check1 <- ("SAT.I.Math", "Translated.SAT.I.Math","ACT.Math.Verified")


QRADM_1618 %>%
        filter_at(.vars = vars(one_of("SAT.I.Math")),
                  ~ !is.na(.))  %>%
        group_by(Sex) %>%
        summarise(avg = mean(SAT.I.Math))

QRADM_1618 %>%
        filter_at(.vars = vars(one_of("Translated.SAT.I.Math")),
                  ~ !is.na(.))  %>%
        group_by(Sex) %>%
        summarise(avg = mean(Translated.SAT.I.Math))
        
QRADM_1618 %>%
        filter_at(.vars = vars(one_of("ACT.Math.Verified")),
                  ~ !is.na(.))  %>%
        group_by(Sex) %>%
        summarise(avg = mean(ACT.Math.Verified))

plot(QRADM_1618$SAT.I.Math, QRADM_1618$ACT.Math.Verified)
plot(QRADM_1618$Translated.SAT.I.Math, QRADM_1618$ACT.Math.Verified)


sat <- na.gam.replace (as.data.frame(QRADM_1618$SAT.I.Math))
act<- na.gam.replace (as.data.frame(QRADM_1618$ACT.Math.Verified))

QR2 <- QRADM_1618
QR2$SATnew <- QR2$Translated.SAT.I.Math
QR2$SATnew[is.na(QR2$SATnew)] <- mean(QR2$Translated.SAT.I.Math, na.rm = TRUE)

QR_1618new <- add_column(QR_1618, act) #Can't add the scores in the 2016-2018 QR dataset because our SAT/ACT scores have 690 rows but out data has 678 rows

# plot(sat$`QRADM_1618$SAT.I.Math`,act$`QRADM_1618$ACT.Math.Verified`)
# plot(act$`QRADM_1618$ACT.Math.Verified`,sat$`QRADM_1618$SAT.I.Math`)

install.packages('fastDummies')
dummydf <- dummy_cols(QR_1618, select_columns = 'Region') #added 7 new dummy variables in the new dataset called dummydf


#do it on majors.
#predict

hist(log(QR_1618$Grade))
hist(sqrt(QR_1618$Grade))





#Friday, 2 OCt




sum(!is.na(QR2$ACT.Math.Verified)) #85 entries for ACT
sum(!is.na(QR2$SAT.R.Math.Verified)) #161 entries for SAT
#Hence, it's a good idea to convert ACT scores into SAT as well. The more the merrier.

a=which(QR2$ACT.Math.Verified==36) #Indexing, which students have an ACT score of 36. Concordance is 36->800.
QR2$Translated.SAT.I.Math[a] <- 800 #This will replace all the SAT scores to 800 of the candidates that have an ACT score of 36. 

b=which(QR2$ACT.Math.Verified==35) #Concordance is 35->780.
QR2$Translated.SAT.I.Math[b] <- 780


c=which(QR2$ACT.Math.Verified==34) #Concordance is 34->760.
QR2$Translated.SAT.I.Math[c] <- 760
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==33)] <- 740
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==32)] <- 720
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==31)] <- 710
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==30)] <- 700
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==29)] <- 680
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==28)] <- 660
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==27)] <- 640
QR2$Translated.SAT.I.Math[which(QR2$ACT.Math.Verified==26)] <- 610

write.csv(QR2,'QR2.csv')

#Replaced all ACT-SAT concordance. Have to rearrange existing values.


#Dropping colums that have all NA values:
QR2 <- QR2[-c(14,18:20,26)]
which(QR2$AP.Computer.Science..A..Score==4)
which(QR2$AP.Computer.Science..A..Score==5)

        
