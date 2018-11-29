library(Rcpp)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(ISLR)
library(MASS)
library('ROSE')
library(caret)

#Read data file

mydata <- read.csv("ABCD_states.csv",header=T)
mydata$no_show <- as.factor(mydata$no_show)
set.seed(123)
ind <- sample(2,nrow(mydata),replace=TRUE, prob = c(0.7,0.3))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

dim(training)
overtrain <- ovun.sample(no_show~., data=training, method="over", N=119450)$data
overtest <- ovun.sample(no_show~., data=testing, method="over", N=119450)$data
head(overtest)

#Decision tree with party
library(party)
mytree <- ctree(no_show~difference+sms_received+alcoholism+age+gender, data=overtrain, controls=ctree_control(mincriterion=0.65, minsplit=20000,minbucket = 7000))
print(mytree)
plot(mytree,type="simple")

preds <- as.numeric(predict(mytree))
act <- as.numeric(overtest$no_show)
class(preds)
class(act)
#Misclassification error
tab<-table(predict(mytree), overtest$no_show)
print(tab)
1-sum(diag(tab))/sum(tab)

class(overtest$no_show)
ttest <- t.test(preds,act)
ttest
cm<-confusionMatrix(as.factor(preds),as.factor(act))
cm$byClass

f1 <- cm$byClass['F1']
pre <- cm$byClass['Precision']
rec <- cm$byClass['Recall']

cat('Precision is',pre)
cat('Recall is',rec)
cat('F1 Score is ',f1)
