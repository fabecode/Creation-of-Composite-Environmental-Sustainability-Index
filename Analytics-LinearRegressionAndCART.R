#Import relevant modules#
setwd("~/Documents/0. UNI WORK!!/6. Analytics I") #remember to set to your working directory 
library(data.table)
library(rpart)
library(rpart.plot)
library(e1071)
library(corrplot)
library(car)
library(ggplot2)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Data Cleaning#
dataset <- fread("FinalDataset.csv",header=T,stringsAsFactors=TRUE)
dataset[dataset == ""] <- NA
summary(dataset)
dataset2<-dataset[,c(4:5,12:17,20:21):=NULL] #remove column that have >=67 NA or at least a country has missing data for the whole 10 yr period
summary(dataset2)


#Split dataset to train-test (70 to 30 split by years)
set.seed(3)
trainset <- subset(dataset2, Year<=2016)
testset <- subset(dataset2, Year>2016)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Explore the CART Model#
#CART Base Model with Country
set.seed(3)
envcart<-rpart(GDPperCap~.-Year,method="anova",control=rpart.control(minsplit=2,cp=0),data=trainset)
rpart.plot(envcart)

#Pruning
CVerror.cap <- envcart$cptable[which.min(envcart$cptable[,"xerror"]), "xerror"] + envcart$cptable[which.min(envcart$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (envcart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(envcart$cptable[i,1] * envcart$cptable[i-1,1]), 1)
plotcp(envcart)
optimal<-prune(envcart,cp=cp.opt)

rpart.plot(optimal,nn=T,main="Optimal CART Model with Country")
optimal$variable.importance
print(optimal)

#Test
predict1<-predict(optimal,testset,type="vector") 
predictresults1<-testset  # Create separate table for prediction results
predictresults1[,Prediction:=predict1] 
options(scipen=999) # Remove scientific number
predictresults1[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresults1[,AbsErrors:=abs(GDPperCap-Prediction)]
predictresults1[,(.N)]
predict1RMSE<-predictresults1[,sqrt(mean(SquaredErrors))]
predict1RMSE
predict1MAE<-predictresults1[,sum(AbsErrors)]/predictresults1[,(.N)]
mean(predict1MAE)/mean(predictresults1$GDPperCap)
mean(predict1RMSE)/mean(predictresults1$GDPperCap)

#CART Base Model without Country
set.seed(3)
envcart_NC<-rpart(GDPperCap~.-Country-Year,method="anova",control=rpart.control(minsplit=2,cp=0),data=trainset)
rpart.plot(envcart_NC)

#Pruning
CVerror.cap <- envcart_NC$cptable[which.min(envcart_NC$cptable[,"xerror"]), "xerror"] + envcart_NC$cptable[which.min(envcart_NC$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (envcart_NC$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(envcart_NC$cptable[i,1] * envcart_NC$cptable[i-1,1]), 1)
plotcp(envcart_NC)
optimal_NC<-prune(envcart_NC,cp=cp.opt)

rpart.plot(optimal_NC,nn=T,main="Optimal CART Model without Country")
optimal_NC$variable.importance
print(optimal_NC)

#Test
predict2<-predict(optimal_NC,testset,type="vector") 
predictresults2<-testset 
predictresults2[,Prediction:=predict2]
options(scipen=999) #Remove scientific number
View(predictresults2)
predictresults2[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresults2[,AbsErrors:=abs(GDPperCap-Prediction)]
predict2RMSE<-predictresults2[,sqrt(mean(SquaredErrors))]
predict2RMSE
predict2MAE<-predictresults2[,sum(AbsErrors)]/predictresults2[,(.N)]
mean(predict2MAE)/mean(predictresults2$GDPperCap)
mean(predict2RMSE)/mean(predictresults2$GDPperCap)

#Create 3 sets of dataset based on low, medium, high GDP per Capita
dataset3<-dataset[dataset2$GDPperCap<20000]
dataset3
dataset4<-dataset[dataset2$GDPperCap<40000&dataset$GDPperCap>=20000]
dataset4
dataset5<-dataset[dataset2$GDPperCap>=40000]
dataset5

#Split dataset to train-test (70 to 30 split by years)
trainsetL<-subset(dataset3,Year<=2016)
testsetL <- subset(dataset3, Year>2016)
trainsetM<-subset(dataset4,Year<=2016)
testsetM <- subset(dataset4, Year>2016)
trainsetH<-subset(dataset5,Year<=2016)
testsetH <- subset(dataset5, Year>2016)

#CART Model for low GDP
set.seed(3)
envmodelL<-rpart(GDPperCap~.-Country-Year,method="anova",control=rpart.control(minsplit=2,cp=0),data=trainsetL)
rpart.plot(envmodelL)

# Pruning
CVerrorL.cap <- envmodelL$cptable[which.min(envmodelL$cptable[,"xerror"]), "xerror"] + envmodelL$cptable[which.min(envmodelL$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (envmodelL$cptable[i,j] > CVerrorL.cap) {
  i <- i + 1
}
cpL.opt = ifelse(i > 1, sqrt(envmodelL$cptable[i,1] * envmodelL$cptable[i-1,1]), 1)
optimalL<-prune(envmodelL,cp =cpL.opt)
printcp(optimalL)
rpart.plot(optimalL,nn=T,main="Optimal CART Model")
summary(optimalL)
optimalL

#Test
predictL<-predict(optimalL,testsetL,type="vector") #Create prediction
predictresultsL<-testsetL 
predictresultsL[,Prediction:=predictL] 
options(scipen=999) #Remove scientific number
predictresultsL[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresultsL[,AbsErrors:=abs(GDPperCap-Prediction)]
predictresultsL[,(.N)]
predictLRMSE<-predictresultsL[,sqrt(mean(SquaredErrors))]
predictLRMSE
predictLMAE<-predictresultsL[,sum(AbsErrors)]/predictresultsL[,(.N)]
mean(predictLMAE)/mean(predictresultsL$GDPperCap)
mean(predictLRMSE)/mean(predictresultsL$GDPperCap)

#CART Model for median GDP
set.seed(3)
envmodelM<-rpart(GDPperCap~.-Country-Year,method="anova",control=rpart.control(minsplit=2,cp=0),data=trainsetM)
rpart.plot(envmodelM)

# Pruning
CVerrorM.cap <- envmodelM$cptable[which.min(envmodelM$cptable[,"xerror"]), "xerror"] + envmodelM$cptable[which.min(envmodelM$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (envmodelM$cptable[i,j] > CVerrorM.cap) {
  i <- i + 1
}
cpM.opt = ifelse(i > 1, sqrt(envmodelM$cptable[i,1] * envmodelM$cptable[i-1,1]), 1)
optimalM<-prune(envmodelM,cp=cpM.opt)
printcp(optimalM)
rpart.plot(optimalM,nn=T,main="Optimal CART Model")
summary(optimalM)
optimalM

#Testing
predictM<-predict(optimalM,testsetM,type="vector") #Create prediction
predictresultsM<-testsetM # Create prediction table 1
predictresultsM[,Prediction:=predictM] #Add predictions to prediction table 1
options(scipen=999) #Remove scientific number
predictresultsM[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresultsM[,AbsErrors:=abs(GDPperCap-Prediction)]
predictresultsM[,(.N)]
predictMRMSE<-predictresultsM[,sqrt(mean(SquaredErrors))]
predictMRMSE
predictMMAE<-predictresultsM[,sum(AbsErrors)]/predictresultsM[,(.N)]
mean(predictMMAE)/mean(predictresultsM$GDPperCap)
mean(predictMRMSE)/mean(predictresultsM$GDPperCap)

#CART Model for high GDP
set.seed(3)
envmodelH<-rpart(GDPperCap~.-Country-Year,method="anova",control=rpart.control(minsplit=2,cp=0),data=trainsetH)
rpart.plot(envmodelH)

#Pruning
CVerrorH.cap <- envmodelH$cptable[which.min(envmodelH$cptable[,"xerror"]), "xerror"] + envmodelH$cptable[which.min(envmodelH$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (envmodelH$cptable[i,j] > CVerrorH.cap) {
  i <- i + 1
}
cpH.opt = ifelse(i > 1, sqrt(envmodelH$cptable[i,1] * envmodelH$cptable[i-1,1]), 1)
optimalH<-prune(envmodelH,cp=cpH.opt)
printcp(optimalH)
rpart.plot(optimalH,nn=T,main="Optimal CART Model")
summary(optimalH)
optimalH

#Testing
predictH<-predict(optimalH,testsetH,type="vector") #Create prediction
predictresultsH<-testsetH 
predictresultsH[,Prediction:=predictH] 
options(scipen=999) #Remove scientific number
predictresultsH[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresultsH[,AbsErrors:=abs(GDPperCap-Prediction)]
predictresultsH[,(.N)]
predictHRMSE<-predictresultsH[,sqrt(mean(SquaredErrors))]
predictHRMSE
predictHMAE<-predictresultsH[,sum(AbsErrors)]/predictresultsH[,(.N)]
mean(predictHMAE)/mean(predictresultsH$GDPperCap)
mean(predictHRMSE)/mean(predictresultsH$GDPperCap)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Explore Linear Regression (LR) Model#
# Repeat Data Cleaning steps to clear CART's changes
dataset <- fread("FinalDataset.csv",header=T,stringsAsFactors=TRUE)
dataset[dataset == ""] <- NA
summary(dataset)
dataset2<-dataset[,c(4:5,12:17,20:21):=NULL] #remove column that have >=67 NA or at least a country has missing data for the whole 10 yr period
summary(dataset2)

#determine the column names that contain NA values
nm <- names(dataset2)[colSums(is.na(dataset2)) != 0]
#replace with the mean - by 'Country'
dataset2[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}), by = Country]

#Split dataset to train-test (70 to 30 split by years)
set.seed(3)
trainset <- subset(dataset2, Year<=2016)
testset <- subset(dataset2, Year>2016)

#Factor categorical data
dataset2$Year = factor(dataset2$Year)
trainset$Year = factor(trainset$Year)
testset$Year = factor(testset$Year)


#Plot histogram of all variables
par(mfrow=c(3,4)) 
hist(dataset2$CO2Prod, xlab='CO2Prod', main = 'Histogram of CO2Prod')
hist(dataset2$EnInt, xlab='EntInt', main = 'Histogram of EntInt')
hist(dataset2$EnSup, xlab='EnSup', main = 'Histogram of EnSup')
hist(dataset2$ReEng1, xlab='ReEng1', main = 'Histogram of ReEng1')
hist(dataset2$ReElect, xlab='ReElect', main = 'Histogram of ReElect')
hist(dataset2$ReEng2, xlab='ReEng2', main = 'Histogram of ReEng2')
hist(dataset2$Biomass, xlab='Biomass', main = 'Histogram of Biomass')
hist(dataset2$PestSal, xlab='PestSal', main = 'Histogram of PestSal')
hist(dataset2$PopPM, xlab='PopPM', main = 'Histogram of PopPM')
hist(dataset2$EnvTax, xlab='EnvTax', main = 'Histogram of EnvTax')
hist(dataset2$PopDens, xlab='PopDens', main = 'Histogram of PopDens')
hist(dataset2$GDPperCap, xlab='GDPperCap', main = 'Histogram of GDPperCap')
par(mfrow=c(1,1)) 

#Check skewness
hist(dataset2$GDPperCap)
hist(log(dataset2$GDPperCap))
skewness(dataset2$GDPperCap)
sapply(dataset2[,3:14], function(x) ifelse(skewness(x) > 1 | skewness(x) < -1, skewness(x), NA))
sapply(dataset2[,3:14], function(x) skewness(x))

#Create logdataset for testing purposes
logdataset <-dataset2
logdataset$CO2Prod = log(logdataset$CO2Prod)
logdataset$EnInt = log(logdataset$EnInt)
logdataset$EnSup = log(logdataset$EnSup)
logdataset$ReEng2 = log(logdataset$ReEng2)
logdataset$PestSal = log(logdataset$PestSal)
logdataset$PopDens = log(logdataset$PopDens)
logdataset$GDPperCap = log(logdataset$GDPperCap)

#Check skewness after applying logarithmic transformation
sapply(logdataset[,3:14], function(x) ifelse(skewness(x) > 1 | skewness(x) < -1, skewness(x), NA))
sapply(logdataset[,3:14], function(x) skewness(x))


#Plot histogram
par(mfrow=c(1,2)) 
hist(dataset2$GDPperCap, xlab='GDPperCap', main = 'Histogram of GDPperCap')
hist(log(dataset2$GDPperCap), xlab='log(GDPperCap)', main = 'Histogram of log(GDPperCap)')

#Plot histogram before logarithmic transformation
par(mfrow=c(2,3)) 
hist(dataset2$CO2Prod, xlab='CO2Prod', main = 'Histogram of CO2Prod')
hist(dataset2$EnInt, xlab='EntInt', main = 'Histogram of EntInt')
hist(dataset2$ReEng2, xlab='ReEng2', main = 'Histogram of ReEng2')
hist(dataset2$EnSup, xlab='EnSup', main = 'Histogram of EnSup')
hist(dataset2$PestSal, xlab='PestSal', main = 'Histogram of PestSal')
hist(dataset2$PopDens, xlab='PopDens', main = 'Histogram of PopDens')

#Plot histogram after logarithmic transformation
par(mfrow=c(2,3))
hist(log(dataset2$CO2Prod), xlab='log(CO2Prod)', main = 'Histogram of log(CO2Prod)')
hist(log(dataset2$EnInt), xlab='log(EnInt)', main = 'Histogram of log(EnInt)')
hist(log(dataset2$EnSup), xlab='log(EnSup)', main = 'Histogram of log(EnSup)')
hist(log(dataset2$ReEng2), xlab='log(ReEng2)', main = 'Histogram of log(ReEng2)')
hist(log(dataset2$PestSal), xlab='log(PestSal)', main = 'Histogram of log(PestSal)')
hist(log(dataset2$PopDens), xlab='log(PopDens)', main = 'Histogram of log(PopDens)')
par(mfrow=c(1,1))

#Look at correlation
str(dataset2[,1:14]) # to check the datatypes
cor(dataset2[,3:14])[,12] # correlation of all variables before log against GDPpercap
corrplot(cor(dataset2[,3:14]), type = "upper") # corrplot of all variables
cor(logdataset[,3:14])[,12] # correlation of all variables after log against GDPpercap
corrplot(cor(logdataset[,3:14]), type = "upper") # corrplot of all variables


#Preliminary Analysis of Model Diagnostic Plots
par(mfrow = c(2,2))
m1 <- lm(GDPperCap ~ CO2Prod, data = dataset2)
plot(m1)

par(mfrow = c(2,2))
m1 <- lm(log(GDPperCap) ~ CO2Prod, data = dataset2)
plot(m1)

par(mfrow = c(2,2))
m1 <- lm(log(GDPperCap) ~ log(CO2Prod), data = dataset2) # Most suitable
plot(m1)

par(mfrow = c(2,2))
m1 <- lm(log(GDPperCap) ~ log(CO2Prod) + Country, data = dataset2)
plot(m1)

#Final Analysis of Model Diagnostic Plots
par(mfrow = c(2,2))
m1 <- lm(log(GDPperCap) ~ log(CO2Prod), data = dataset2)
plot(m1)

par(mfrow = c(2,2))
m2 <- lm(log(GDPperCap) ~ log(EnInt), data = dataset2)
plot(m2)

par(mfrow = c(2,2))
m3 <- lm(log(GDPperCap) ~ log(EnSup), data = dataset2)
plot(m3)

par(mfrow = c(2,2))
m4 <- lm(log(GDPperCap) ~ ReEng1, data = dataset2)
plot(m4)

par(mfrow = c(2,2))
m5 <- lm(log(GDPperCap) ~ ReElect, data = dataset2)
plot(m5)

par(mfrow = c(2,2))
m6 <- lm(log(GDPperCap) ~ log(ReEng2), data = dataset2)
plot(m6)

par(mfrow = c(2,2))
m7 <- lm(log(GDPperCap) ~ Biomass, data = dataset2)
plot(m7)

par(mfrow = c(2,2))
m8 <- lm(log(GDPperCap) ~ log(PestSal), data = dataset2)
plot(m8)

par(mfrow = c(2,2))
m9 <- lm(log(GDPperCap) ~ PopPM, data = dataset2)
plot(m9)

par(mfrow = c(2,2))
m10 <- lm(log(GDPperCap) ~ EnvTax, data = dataset2)
plot(m10)

par(mfrow = c(2,2))
m11 <- lm(log(GDPperCap) ~ log(PopDens) + Country, data = dataset2)
plot(m11)

#Regression to find multicollinearity
set.seed(3)
VifReg<-lm(log(GDPperCap) ~ log(CO2Prod) + log(EnInt) + log(EnSup) + ReEng1 + ReElect + log(ReEng2) + Biomass+ log(PestSal) + PopPM + EnvTax + log(PopDens), data = dataset2)
vif(VifReg)

#------------------------------------------------------------------------------------------------------------------------------
#REGRESSION
#Regression on trainset for factored Country

RegTrainSet <- lm(log(GDPperCap) ~ log(CO2Prod) + log(EnInt) + log(EnSup) + ReEng1 + ReElect + log(ReEng2) + Biomass+ log(PestSal) + PopPM + EnvTax + log(PopDens) + Country, data = trainset)
summary(RegTrainSet)
step(RegTrainSet) #backward elimination

RegTrainSet2 <- lm(formula = log(GDPperCap) ~ log(CO2Prod) + log(EnInt) + log(ReEng2) + Biomass + PopPM + EnvTax + log(PopDens) + Country, data = trainset) #AIC=-1197.63
summary(RegTrainSet2)
residuals(RegTrainSet2) 

#Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.RegTrainSet2.train <- sqrt(mean(residuals(RegTrainSet2)^2))  
RMSE.RegTrainSet2.train #0.0351469

#Apply model from trainset to predict on testset.
predict.RegTrainSet2.test <- predict(RegTrainSet2, newdata = testset)
predictresult.RegTrainSet2.test <-testset # Create prediction table 1
predictresult.RegTrainSet2.test[,Prediction:=exp(1)^predict.RegTrainSet2.test] #Add predictions to prediction table 1
options(scipen=999) #Remove scientific number
View(predictresult.RegTrainSet2.test)
predictresult.RegTrainSet2.test[,SquaredErrors:=(GDPperCap-Prediction)^2]     
predictresult.RegTrainSet2.test[,AbsErrors:=abs(GDPperCap-Prediction)]
RMSE.RegTrainSet2.test<-predictresult.RegTrainSet2.test[,sqrt(mean(SquaredErrors))]
RMSE.RegTrainSet2.test  #4131.066
MAE.RegTrainSet2.test<-predictresult.RegTrainSet2.test[,sum(AbsErrors)]/predictresult.RegTrainSet2.test[,(.N)]
mean(MAE.RegTrainSet2.test)/mean(predictresult.RegTrainSet2.test$GDPperCap) #0.06894254
mean(RMSE.RegTrainSet2.test)/mean(predictresult.RegTrainSet2.test$GDPperCap) #0.1150013

#RMSE errors with factored country
RMSE.RegTrainSet2.train #0.0351469
RMSE.RegTrainSet2.test  #4131.066

#Regression on trainset without factored Country
RegTrainSetB <- lm(log(GDPperCap) ~ log(CO2Prod) + log(EnInt) + log(EnSup) + ReEng1 + ReElect + log(ReEng2) + Biomass+ log(PestSal) + PopPM + EnvTax + log(PopDens), data = trainset)
summary(RegTrainSetB)
step(RegTrainSetB) #backward elimination

RegTrainSetB2 <- lm(log(GDPperCap) ~ log(CO2Prod) + log(EnInt) + log(EnSup) + ReElect + log(ReEng2) + Biomass + log(PestSal) + PopPM + EnvTax + log(PopDens), data = trainset) #AIC=-548.59
summary(RegTrainSetB2)
residuals(RegTrainSetB2) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.RegTrainSetB2.train <- sqrt(mean(residuals(RegTrainSetB2)^2))  
RMSE.RegTrainSetB2.train

# Apply model from trainset to predict on testset.
predict.RegTrainSetB2.test <- predict(RegTrainSetB2, newdata = testset)
predictresult.RegTrainSetB2.test <-testset # Create prediction table 1
predictresult.RegTrainSetB2.test[,Prediction:=exp(1)^predict.RegTrainSetB2.test] #Add predictions to prediction table 1
options(scipen=999) #Remove scientific number
View(predictresult.RegTrainSetB2.test)
predictresult.RegTrainSetB2.test[,SquaredErrors:=(GDPperCap-Prediction)^2]
predictresult.RegTrainSetB2.test[,AbsErrors:=abs(GDPperCap-Prediction)]
RMSE.RegTrainSetB2.test<-predictresult.RegTrainSetB2.test[,sqrt(mean(SquaredErrors))]
RMSE.RegTrainSetB2.test           #12577.92
MAE.RegTrainSetB2.test<-predictresult.RegTrainSetB2.test[,sum(AbsErrors)]/predictresult.RegTrainSetB2.test[,(.N)]
mean(MAE.RegTrainSetB2.test)/mean(predictresult.RegTrainSetB2.test$GDPperCap)   
mean(RMSE.RegTrainSetB2.test)/mean(predictresult.RegTrainSetB2.test$GDPperCap)

#RMSE errors without factored country
RMSE.RegTrainSetB2.train #0.2210238
RMSE.RegTrainSetB2.test  #12577.92

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comparison between 7 models#
#5 CART Models
mean(predict1MAE)/mean(predictresults1$GDPperCap)   #8.6%
mean(predict1RMSE)/mean(predictresults1$GDPperCap)  #11.7%

mean(predict2MAE)/mean(predictresults2$GDPperCap)   #17.58%
mean(predict2RMSE)/mean(predictresults2$GDPperCap)  #25.84%

mean(predictLMAE)/mean(predictresultsL$GDPperCap)   #17.36%
mean(predictLRMSE)/mean(predictresultsL$GDPperCap)  #20.67%

mean(predictMMAE)/mean(predictresultsM$GDPperCap)   #7.36%
mean(predictMRMSE)/mean(predictresultsM$GDPperCap)  #8.53%

mean(predictHMAE)/mean(predictresultsH$GDPperCap)   #11.95%
mean(predictHRMSE)/mean(predictresultsH$GDPperCap)  #17.56%

#2 LR Models
mean(MAE.RegTrainSet2.test)/mean(predictresult.RegTrainSet2.test$GDPperCap)    #6.89%
mean(RMSE.RegTrainSet2.test)/mean(predictresult.RegTrainSet2.test$GDPperCap)   #11.50%

mean(MAE.RegTrainSetB2.test)/mean(predictresult.RegTrainSetB2.test$GDPperCap)  #22.39%
mean(RMSE.RegTrainSetB2.test)/mean(predictresult.RegTrainSetB2.test$GDPperCap) #35.01%

#Best model selected is RegTrainSet2 (LG with Country)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Creation of Sustainability Index#
#Assigning Weights based on the coefficient in the best model selected
weightage <- list(round((abs(coefficients(RegTrainSet2)[2:8]))/sum(abs(coefficients(RegTrainSet2)[2:8])),3))
weightage

#Create the Sustainability index
scaling <- data.table(log_CO2Prod=log(dataset2$CO2Prod),log_EnInt=log(dataset2$EnInt),log_ReEng2=log(dataset2$ReEng2),Biomass=dataset2$Biomass,PopPM=dataset2$PopPM,EnvTax=dataset2$EnvTax,log_PopDens=log(dataset2$PopDens))
scaling <- data.frame(lapply(scaling, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100))) #to scale from 0 to 100 for each column 
dataset2$SustainIndex <- round((scaling$log_CO2Prod*0.128)+(scaling$log_EnInt*0.257)+(scaling$log_ReEng2*0.042)+(scaling$Biomass*0.001)+(scaling$PopPM*0.007)+(scaling$EnvTax*0.046)+(scaling$log_PopDens*0.520),2)

#Distribution of the Sustainability index
summary(dataset2$SustainIndex)
par(mfrow = c(1,1))
hist(dataset2$SustainIndex, ylab = "Sustainability Index") 
skewness(dataset2$SustainIndex) #no need to log

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis of the Sustainability Index#
Analysis <- lm(log(GDPperCap) ~ Country+Year+SustainIndex, data = dataset2)
summary(Analysis) #multiple r square is lower, residual standard error is also higher
summary(RegTrainSet2) #Model before sustainability index is created

#General relationship
ggplot(data=dataset2, aes(x=SustainIndex, y=GDPperCap)) + geom_point() + geom_smooth(method = "lm") + labs(title="Relationship between Sustainability Index & GDP per capita")

#Relationship by Country
col<-c('black','forestgreen', 'red2', 'orange', 'cornflowerblue', 
       'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue', 
       'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', 'tan3',
       'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 
       'seagreen1', 'moccasin', 'mediumvioletred','cadetblue1',
       "darkolivegreen1" ,   "tomato3" , "#7CE3D8","yellow2")
ggplot(data=dataset2, aes(x=SustainIndex, y=GDPperCap, color=Country)) + geom_point() + geom_smooth(method = "lm") + scale_color_manual(values=col) + labs(title="Relationship between Sustainability Index & GDP per capita by Country")
#Look at each country
ggplot(data=dataset2[Country=="Romania"], aes(x=SustainIndex, y=GDPperCap, color=Country)) + geom_point() + geom_smooth(method = "lm") +scale_color_manual(values="cadetblue1")+ labs(title="Relationship between Sustainability Index & GDP per capita in Romania")
ggplot(data=dataset2[Country=="Sweden"], aes(x=SustainIndex, y=GDPperCap, color=Country)) + geom_point() + geom_smooth(method = "lm") +scale_color_manual(values="yellow2")+ labs(title="Relationship between Sustainability Index & GDP per capita in Sweden")
ggplot(data=dataset2[Country=="Cyprus"], aes(x=SustainIndex, y=GDPperCap, color=Country)) + geom_point() + geom_smooth(method = "lm") +scale_color_manual(values="cornflowerblue")+ labs(title="Relationship between Sustainability Index & GDP per capita in Cyprus")

#Relationship by Year
ggplot(data=dataset2, aes(x=SustainIndex, y=GDPperCap)) + geom_point() + facet_wrap(~Year, ncol = 2) + geom_smooth(method = "lm") + labs(title="Relationship between Sustainability Index & GDP per capita by Year")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#END#



