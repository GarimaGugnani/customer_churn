#Set Directory
getwd()
setwd("D:/R files/PM-Project")
cellphone=read.csv("Cellphone.csv", sep=",", header=TRUE)
#Understand Data
dim(cellphone)
names(cellphone)
str(cellphone)
cellphone$Churn=as.factor(cellphone$Churn)
cellphone$ContractRenewal=as.factor(cellphone$ContractRenewal)
cellphone$DataPlan=as.factor(cellphone$DataPlan)
summary(cellphone)
View(cellphone)
# Checking null data
sapply(data,function(x) sum(is.na(x)))
#univariate analysis
boxplot(cellphone$AccountWeeks)
boxplot(cellphone$DayMins)
boxplot(cellphone$DayCalls)
boxplot(cellphone$MonthlyCharge)
boxplot(cellphone$OverageFee)
boxplot(cellphone$RoamMins)
library(ggplot2)
ggplot(cellphone, aes(x=Churn, 
                      y=..count../sum(..count..)))+geom_bar()+
  labs(x="Churn", y="Percent", title="Customer Churn")+scale_y_continuous(labels = scales::percent)
ggplot(cellphone, aes(x=ContractRenewal, 
                     y=..count../sum(..count..)))+geom_bar()+
  labs(x="Contract Renewal", y="Percent")+scale_y_continuous(labels = scales::percent)
ggplot(cellphone, aes(x=DataPlan, 
                      y=..count../sum(..count..)))+geom_bar()+
  labs(x="Data Plan", y="Percent")+scale_y_continuous(labels = scales::percent)



ggplot(cellphone, aes(x=MonthlyCharge, y=DayMins))+geom_point()
ggplot(cellphone, aes(x=MonthlyCharge, y=DataPlan))+geom_point()
ggplot(cellphone, aes(x=DataUsage, y=MonthlyCharge))+geom_point()

install.packages("corrplot")
library(corrplot)

cellphone.cor=cor(cellphone[,-c(1,3,4)])
cellphone.cor
corrplot(cellphone.cor)
pairs(cellphone[,-c(1,3,4)])
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cellphone.cor, col = palette, symm = TRUE)


library(caTools)
split <- sample.split(cellphone$Churn, SplitRatio = 0.7)
#we are splitting the data such that we have 70% of the data is Train Data and 30% of the data is my Test Data

train<- subset(cellphone, split == TRUE)
test<- subset( cellphone, split == FALSE)

table(train$Churn)
table(test$Churn)


model1=glm(Churn~., data=train, family="binomial")
model1
summary(model1)

# Check for multicollinearity
library(carData)
vif(model1)

model2=glm(Churn~., data=train[,-9], family=binomial(link="logit"))
summary(model2)

vif(model2)

model3=glm(Churn~., data=train[,-c(5,9)], family="binomial")
summary(model3)

vif(model3)


model4=glm(Churn~., data=train[,-c(2,5,9)], family="binomial")
summary(model4)

vif(model4)



library(blorr)
#AIC=Alkaline information criteria
blr_step_aic_both(model1, details = TRUE)
final_model=glm(Churn~CustServCalls+ContractRenewal+DayMins+DataPlan+OverageFee+RoamMins, data=train, family="binomial")
summary(final_model)

predtrain_log=predict(final_model, data=train, type="response")
predtest_log=predict(final_model, newdata=test, type="response")

table(train$Churn, predtrain_log>0.5)
Accuracy=(1944+64)/2333
Accuracy
sensitivity=64/(64+274)
sensitivity
specificity=1944/(1944+51)
specificity


table(test$Churn, predtest_log>0.5)
Accuracy=(832+31)/1000
Accuracy
sensitivity=31/(31+114)
sensitivity
specificity=832/(832+23)
specificity


library(ROCR)
library(ineq)
library(InformationValue)
install.packages("InformationValue")

predobjtrain = prediction (predtrain_log, train$Churn)
perftrain = performance(predobjtrain, "tpr", "fpr")
plot(perftrain)#ROC curve

preobjtest=prediction(predtest_log,test$Churn)
preftest=performance(preobjtest,"tpr","fpr")
plot(preftest)

auc = performance(predobjtrain, "auc")
auc = as.numeric(auc@y.values)
auc

auctest=performance(preobjtest,"auc")
auctest=as.numeric(auctest@y.values)
auctest


KStrain=max(perftrain@y.values[[1]]-perftrain@x.values[[1]])
KStrain

KStest=max(preftest@y.values[[1]]-preftest@x.values[[1]])
KStest#same?

Ginitrain=ineq(predtrain_log, "gini")
Ginitrain

Ginitest=ineq(predtest_log, "gini")
Ginitest


############KNN##############

#Normalising data



normalize<-function(x){ return((x-min(x))/(max(x)-min(x)))}
norm_data=as.data.frame(lapply(cellphone[,-c(1,3,4)], normalize))
summary(norm_data)
usable_data = cbind(cellphone[,c(1,3,4)], norm_data)
View(usable_data)
summary(usable_data)

library(caTools)
set.seed(10)
split <- sample.split(usable_data$Churn, SplitRatio = 0.7)
#we are splitting the data such that we have 70% of the data is Train Data and 30% of the data is my Test Data

norm_train<- subset(usable_data, split == TRUE)
norm_test<- subset(usable_data, split == FALSE)

dim(norm_train)
dim(norm_test)

library(class)

predKNN=knn(norm_train[,-1], norm_test[,-1], norm_train[,1], k=48)
table.knn1=table(norm_test[,1],predKNN)
sum(diag(table.knn1)/sum(table.knn1)) 

predKNN2=knn(norm_train[,-1], norm_test[,-1], norm_train[,1], k=35)
table.knn2=table(norm_test[,1],predKNN2)
sum(diag(table.knn2)/sum(table.knn2)) 

predKNN3=knn(norm_train[,-1], norm_test[,-1], norm_train[,1], k=30)
table.knn3=table(norm_test[,1],predKNN3)
sum(diag(table.knn3)/sum(table.knn3)) 


predKNN4=knn(norm_train[,-1], norm_test[,-1], norm_train[,1], k=10)
table.knn4=table(norm_test[,1],predKNN4)
sum(diag(table.knn4)/sum(table.knn4)) 

table.knn4
Accuracy_KNN=sum(diag(table.knn4)/sum(table.knn4))
Accuracy_KNN
sensitivity_KNN=61/(61+84)
sensitivity_KNN
specificity_KNN=846/(846+9)
specificity_KNN


#######NAIVES BAYES############
install.packages("e1071")
library(e1071)
NB=naiveBayes(Churn~., data=norm_train)
predNB=predict(NB, norm_test,type="class")
tab.NB=table(norm_test[,1],predNB)
tab.NB
Accuracy_NB=sum(diag(tab.NB)/sum(tab.NB))
Accuracy_NB
sensitivity_KNN=48/(48+97)
sensitivity_KNN
specificity_KNN=832/(832+23)
specificity_KNN
