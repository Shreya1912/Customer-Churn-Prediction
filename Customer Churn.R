rm(list=ls())
library(readxl)
library(Hmisc)
library(MASS)
library(caret)
library(pROC)
library (ROCR)
library(pscl)

#Inputting the dataset
mydata<-read_excel("Data 2.xlsx")
summary(mydata)

#Logistic Regression Model Fitting
mylogit<-glm(Churn~CustomerAge+Joined+AverageSpend,data=mydata,family=binomial(link="logit"))

#Checking for multicollinearity
cor(mydata$CustomerAge, mydata$AverageSpend)
cor(mydata$CustomerAge, mydata$Joined)
cor(mydata$Joined, mydata$AverageSpend)

#Model Summary
summary(mylogit) 

#checking pseudo R^2 (McFadden)
pR2(mylogit)

#Confidence Intervals
#confint(mylogit) 

#exponentiated coefficients
#exp(mylogit$coefficients)

#exponentiated confidence intervals
#exp(confint(mylogit))

AIC(mylogit)

#Assessing the model
preddata<-with(mydata,data.frame(CustomerAge,Joined,AverageSpend))
probchurn<-predict(mylogit,newdata=preddata,type="response")
predchurn<-ifelse(probchurn > 0.5, 1,0)
#Simple_CLV <- 0.5*mydata$AverageSpend*(1/(probchurn))

missclass<-predchurn!=mydata$Churn
#misclasserror<-mean(predchurn!=mydata$Churn)
#print(paste('Accuracy',1-misclasserror))

confMat3<-confusionMatrix(data = as.factor(predchurn),reference = as.factor(mydata$Churn),positive = "1")
confMat3

#Plotting ROC curve and AUC
#roc(mydata$Churn,mylogit$fitted.values, plot=TRUE)
roc(mydata$Churn,mylogit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="FP Rate", ylab="TP Rate",print.auc=TRUE)


Agg_CLV <- c() #Calculating CLV for 1 year

for(i in (1:length(probchurn))){
  dis <- 0.08
  r <- (1-probchurn[i])
  Agg_CLV[i] <- 0
  for (j in 0:2){
    Agg_CLV[i] <- Agg_CLV[i] + ((0.5*mydata$AverageSpend[i]*(r)^j)/((1+dis)^j))
  }
}


finaldata<-cbind(mydata,probchurn,predchurn, missclass, Agg_CLV)
finaldata
#

#Calculating average churn for the those who joined vs did not join

joined <- subset(finaldata, (Joined==1 & Churn == 0),select=c(probchurn, Agg_CLV))
retention_joined <- (1-(joined$probchurn))
clv_joined <- joined$Agg_CLV 
length(retention_joined)

notjoined <- subset(finaldata, (Joined==0 & Churn == 0), select=c(probchurn, Agg_CLV))
retention_notjoined <- (1-(notjoined$probchurn))
#retention_notjoined
clv_notjoined <- notjoined$Agg_CLV
length(retention_notjoined)

#checking equal variance

var.test(x=retention_joined, y=retention_notjoined, alternative = 'two.sided')
var.test(x=clv_joined, y = clv_notjoined, alternative = 'two.sided')

#t.test with alternate hypothesis as joined < not joined
t.test(x=retention_joined, y=retention_notjoined, alternative = 'two.sided', paired = FALSE, var.equal = TRUE)

#t.test with alternate hypothesis as joined < not joined
t.test(x=clv_joined, y=clv_notjoined, alternative = 'two.sided', paired = FALSE, var.equal = TRUE)
