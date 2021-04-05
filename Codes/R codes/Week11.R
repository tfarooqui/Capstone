#Week 11 Capstone Meeting (Fri, 30th Oct)

#make grades ordinal
#Do more research on mice, why chose certian methods over others?

#Go back and seperate individual components of the grades.
#Irats- 10%, Peer eval- 15%, Instructor eval 10%, Writing assignment 10%, Exam 1/2 15%
#Reweigh all of them, Make sure they are all out of 100, take average to get a Scaled grade

library(readr)
data <- read_csv("TalhaQRdata.csv")

Ind_Grd <- function(irat,peer,inst,wa,exam1,exam2) {
  4/3 * (0.1*(irat) + (0.15*peer) + 0.1*(inst) + 0.1*(wa) + (0.15*exam1) + (0.15*exam2))  
}

IndGrd <- Ind_Grd(data$IRAT,data$Peer,data$Instructor,
       data$Writing,data$Exam1,data$Exam2)

sidebysidegrades <- cbind(data,IndGrd)[,c(6,57)]

#Generally, the final grade that includes the team components is higher than individual grades.  (No surprises there!)
#In some rare cases, a person with a higher ind grade hs a smaller final grade.
#There are some NAs in the data too.

#ADM1445 <- read_csv("TalhaQRADM1415.csv") just to have a sense.
#View(ADM1415)
#sapply(ADM1445, function(x) sum(is.na(x))) #SO MUCH MISSING DATA

ADM1415edit <- read_csv("TalhaQRADM1415edit.csv")
sapply(ADM1415edit, function(x) sum(is.na(x))) #Missing values.
colnames(ADM1415edit)[6:7] <- c('SAT','HSG') #Changing names of really long column names...

QRmerged2 <- left_join(data,ADM1415edit, by = "PIN")[,-c(7:58)]
qr1415 <- QRmerged2[(1:334),]
qr1619 <- read_csv("QR2regressiondata.csv")[,-1]
qr1619 <- qr1619[,-7]
colnames(qr1619 )[9:10] <- c('SAT','HSG')
qr1419 <- rbind(qr1415, qr1619)


#Reconverting grades to individual grades
#Prof. Wertz "I’ve thought about the individual grade portion a bit more, 
#            and I think it’s best if you only use the IRATs and the exams"

Ind_Grd2 <- function(irat,exam1,exam2) {
  5/2 * (0.1*(irat) + (0.15*exam1) + (0.15*exam2))  
}

IndGrd2 <- Ind_Grd2(data$IRAT,data$Exam1,data$Exam2)
finaldata <- cbind(qr1419,IndGrd2) #Has all the data from 2014 - 2019. with HSG grades and SAT, and reordered 
#Next, we move the 2 grades next to each other so that we can compare and contrast.
finaldata <- finaldata[, c(1,2,3,4,5,6,11,7,8,9,10)] 
cor(finaldata$Grade,finaldata$IndGrd2, use = "complete.obs") #correlation 0.8733622


data2 <-finaldata %>%
  mutate(
    Sex.x = as.factor(Sex.x),
    Region = as.factor(Region),
    HSG = as.factor(HSG),
    Year= as.factor(Year),
    SAT = as.numeric(SAT),
    Grade = as.numeric(Grade)
  )

#Grade as dependent variable
summary(glm(Grade ~ SAT + HSG, data = data2, family = "gaussian")) 
#Individual Grades as dependent variable
summary(glm(IndGrd2 ~ SAT + HSG, data = data2, family = "gaussian")) 

imp <- mice(data2, maxit=0)
predM = imp$predictorMatrix
meth = imp$method
#ordinal categorical variable
poly <- c("HSG")
meth[poly] = "polr"
# Dichotomous variable
log <- c("Sex.x")
meth[log] = "logreg"
satt <- c("SAT")
meth[satt] = "norm.nob"
im <- mice(data2, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  TRUE)
fitm <- with(im, lm(IndGrd2 ~ HSG + SAT))
summary(fitm)
summary(pool(fitm))


im3 <- mice(data2, maxit = 10, 
            predictorMatrix = predM, 
            method = meth)
fitm2 <- with(im, lm(IndGrd2 ~ HSG + SAT + Region + Sex.x +Year))
summary(fitm2)
summary(pool(fitm2))

### Changing HSG 
df2 <- finaldata
df2$HSG[df2$HSG=="A"]<-1
df2$HSG[df2$HSG=="B"]<-2
df2$HSG[df2$HSG=="C"]<-3
df2$HSG[df2$HSG=="D"]<-4
df2

df2 <-df2 %>%
  mutate(Sex.x = as.factor(Sex.x),
    Region = as.factor(Region),
    Year= as.factor(Year),
    HSG = as.numeric(HSG),
    SAT = as.numeric(SAT),
    IndGrd2 = as.numeric(IndGrd2))

summary(glm(IndGrd2 ~ SAT + HSG, data = df2, family = "gaussian")) 

impp <- mice(df2, maxit=0)
predM = imp$predictorMatrix
meth = imp$method
#ordinal categorical variable
poly <- c("HSG")
meth[poly] = "polr"
# Dichotomous variable
log <- c("Sex.x")
meth[log] = "logreg"
satt <- c("SAT")
meth[satt] = "norm.nob"
impp <- mice(df2, maxit = 5, 
           predictorMatrix = predM, 
           method = meth, print =  TRUE)
fit2m <- with(impp, lm(IndGrd2 ~ HSG + SAT))
summary(fit2m)
summary(pool(fit2m))


#QR19grades not out of 100. They're out of 72.
#Scale them accordingly.
#Running them with categorical variables.
#Majors grouped together.
#Do it first with all seperate.



#Week 12, 6 November
#Replacing NA values by exam 1
which(is.na(finaldata$IndGrd2))
indexing <- c(2,16,48,61,114,119,129,147,664,776,786,878,882,927,975,978)
NAs <- data[indexing,] #indexing, and finding the indexes of all the NAs from our individual grades.
#Creating a new function which replaces the missing exams with the exam taken.
Ind_Grd3 <- function(irat,exam) {
  5/2 * (0.1*(irat) + (0.3*exam))  
}
#combining them into a new dataframe for context 
predgrades <- as.data.frame(c(Ind_Grd3(NAs[1:4,]$IRAT,NAs[1:4,]$Exam2), #96.600 89.975 46.650 65.125
                Ind_Grd3(NAs[5,]$IRAT,NAs[5,]$Exam1), #66.375
                Ind_Grd3(NAs[6:8,]$IRAT,NAs[6:8,]$Exam2), #93.300 76.100 78.775
                Ind_Grd3(NAs[9:11,]$IRAT,NAs[9:11,]$Exam1), # 63.80597 78.25270 58.71227
                Ind_Grd3(NAs[12,]$IRAT,NAs[12,]$Exam2), #77.5
                Ind_Grd3(NAs[13:14,]$IRAT,NAs[13:14,]$Exam1), #52.56406 79.69842
                Ind_Grd3(NAs[15,]$IRAT,NAs[15,]$Exam2), #52.63405
                Ind_Grd3(NAs[16,]$IRAT,NAs[16,]$Exam1)))
colnames(predgrades) <- "Grd"
##combining them into a new dataframe for sude-by-side comparison.
sidebysidegrades2 <- cbind(predgrades,NAs)[,c(1,7)]
testss<-finaldata[indexing,]
testss$IndGrd2 <- predgrades
#gonna export finaldata as a csv
#Manually going to replace the IndGrd2 NA values in the finaldata dataset
write.csv(finaldata,"copyfinaldata.csv")
copyfinaldata1 <- read_csv("copyfinaldata1.csv")
View(copyfinaldata1) 
copyfinaldata1 <- copyfinaldata1[,-1]
#will be using this dataset from now on.
#gonna work on the 19-20 dataset now.
QR19grades <- read_csv("QR19grades.csv")
View(QR19grades) #standardizing the score and reconverting them into a 100 scale, rather than a 72.
QR19grades$Midterm <- ((QR19grades$Midterm)* 100/72)
QR19grades$Final <- ((QR19grades$Final)* 100/72)
#Creating
Ind_Grd2019 <- function(irat,exam1,exam2) {
  a = (0.1*(irat) + (0.15 * exam1) + (0.15 *exam2))
  return(2.5*a)
}
#Applying function
IndGrd2019 <- Ind_Grd2019(QR19grades$IRAT,QR19grades$Midterm,QR19grades$Final)
QR19grades$IndGrd2=IndGrd2019 

#I just realized we haven't done concordance on 2019 data. UGHHHH.
TalhaQRADM <- read_csv("TalhaQRADM.csv")
View(TalhaQRADM)
helPP <- TalhaQRADM[which(TalhaQRADM$Matriculation.Date == 2019),]
helPP$HSG <- NA
helPP$SAT <- NA
write_csv(helPP, "2019adm.csv")
#After doing SAT/ACt concordances and grade conversions, importing the 2019 adm data
ADM19 <-read_csv("2019admHSGSAT.csv")
#removing cols, and switching sat/act positions.
ADM19 <- ADM19[,-c(6:26)]
ADM19 <-  ADM19[, c(1:5,7,6)]
#HAVE TO ADD GRADES T0 THIS.

#Subset 2019 admission data
#merge with grade data 
#create a new df and edit it to copyfinaldata spec.
#then use rowbind to add it to copyfinaldata
#heyyy <- TalhaQRADM[(691:936),]

QRdata2019 <- left_join(QR19grades,ADM19,by = "PIN")
QRdata2019$Year <- "ay1920" #adding in the year column for easy merging later
#dropping unnused cols
QRdata2019 <- QRdata2019[,-c(1:8,10,13)] 
#Missing column region, will add
QRdata2019$Region <- NA
QRdata2019$Major <- NA #because undecided
#Changing colnames, col positions to mimic our copyfinaldata1 dset to merge.
colnames(QRdata2019)[c(1,4)] <- c("Grade","Sex.x")
QRdata2019 <-  QRdata2019[, c(2,4,10,9,11,1,3,5,6,8,7)]
#Vertically binding this 2019 QR dataset with our old copyfinaldata1 set
QRtotaldata <- rbind(copyfinaldata1,QRdata2019)
#exporting as csv
write.csv(QRtotaldata, "regressiondata3nov.csv")

