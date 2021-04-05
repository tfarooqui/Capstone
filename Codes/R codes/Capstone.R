library(readr)
#QR2_nn <- read_csv("~/Documents/QR2_nonames.csv")
#df <- QR2_nn[-c(1,7,9,10)]
#hist(df$Translated.SAT.I.Math)
#sum(!is.na(df$Translated.SAT.I.Math)) #Number of values increased from 161 to 358 after concordance and conversion.

#newdf <- df
#newdf$Translated.SAT.I.Math[is.na(newdf$Translated.SAT.I.Math)]<-mean(newdf$Translated.SAT.I.Math, na.rm = T)
#newdf$Translated.SAT.I.Math <- round(newdf$Translated.SAT.I.Math, digits = -1)

#Creating a new dataframe, and Replacing the NA values with the mean of the SAT values
#Mean is 748.7989, rounded that up to nearest 10 (750) as per SAT rulebook.

#sum(!is.na(df$GCE..A..Levels.H1.MATHEMATICS.Grade)) #40
# sum(!is.na(df$GCE..A..Levels.H2.MATHEMATICS.Grade)) #117
# 
# sum(!is.na(df$IB.Mathematics.SL.Score)) #35
# sum(!is.na(df$IB.Mathematics.HL.Score)) #16
#  
# sum(!is.na(df$AP.Calculus..AB..Score)) #25, 16 for stats.

#Monday 19 OCt
#importing our proper dataset with SAT scores and final predicted grades.
QR1 <- read_csv("QRdatGrades:SAT.csv")
sum(!is.na(QR1$`‘New’ High School Grades.`))  #258 final predicted grades from high-schoolers.
sum(!is.na(QR1$Translated.SAT.I.Math)) #358 Translated SAT/ACT scores.
#Editing datset by removing unused columns.
QRR <- QR1[,-c(1,7,9:22,24)]
write.csv(QRR, 'QR1618SATGRADES.csv')
QR1618 <- read_csv(" QR1618.csv")
#QRmerged <- merge(QR1618,QRR,by="PIN")
QRmerged <- left_join(QR1618,QRR, by = "PIN")
#Preparing our datset for regression by removing unused columns.
QR2 <- QRmerged[,-c(1,8:58)]
#QR2$Sex.x == QR2$Sex.y Join is perfect, all the genders are same for both the dataframes.
#QR2 <- QR2[,-7]
write.csv(QR2, 'QR2regressiondata.csv') #Exporting DF as a csv file.
high <- c("A","B")
low = c("C","D")
QR2$grade <- factor(ifelse(QR2$`‘New’ High School Grades.` %in% high, "high","low"))
QR2$grade <- factor(QR2$grade)
mylogit <- glm(Grade/100 ~ Translated.SAT.I.Math + grade, data = QR2, family = "binomial")

#use family = binomial
#useg lm, use family = gaussian
#predict continuous variable.
#summary(mylogit)
#create dummy variables.
#Remove the rows with NA to check regression
#Use MICE
#SVMs 
#convert categorical to dummy for glm.
#Look into mice to impude missing values. do it again.







