library(neuralnet)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ROSE)
library(Rcpp)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(ISLR)
library(randomForest)
library(caret)
library(e1071)
library(keras)

#install_keras()

data <- read.csv('ABCD_states.csv')
View (data)
head(data)
data$difference <- round((data$difference - min(data$difference))/(max(data$difference) - min(data$difference)),3)
data$age <- round((data$age - min(data$age))/(max(data$age) - min(data$age)),3)
data$neighbourhood <- NULL
data$state <- NULL

head(data)
str(data)
data <- data[,c(10,1,2,3,4,5,6,7,8,9)]
head(data)


hist(data$age)
hist(data$difference)

set.seed(222)
ind <- sample(3, nrow(data), replace = TRUE, prob = c(0.05, 0.1, 0.85))
training <- data[ind==1,]
testing <- data[ind==2,]
dev <- data[ind==3,]
table(training$no_show)
table(testing$no_show)
dim(training)
undertrain <- ovun.sample(no_show~., data=training, method='under', N= 2122)$data
str(undertrain)
#table(undertrain$no_show)
#undertrain$difference <- round((undertrain$difference - min(undertrain$difference))/(max(undertrain$difference) - min(undertrain$difference)),3)
#undertrain$age <- round((undertrain$age - min(undertrain$age))/(max(undertrain$age) - min(undertrain$age)),3)



undertest <- ovun.sample(no_show~., data=testing, method='under', N= 4368)$data
head(undertest)
#un_max_diff <- max(undertest$difference)
#un_min_diff <- min(undertest$difference)
#cat('Range of differences:',un_min_diff,'and',un_max_diff)
#undertest$difference <- round((undertest$difference - min(undertest$difference))/(max(undertest$difference) - min(undertest$difference)),3)
#undertest$age <- round((undertest$age - min(undertest$age))/(max(undertest$age) - min(undertest$age)),3)

table(undertest$no_show)


set.seed(123)

n <- neuralnet(no_show~difference+sms_received+age+gender+scholarship+hipertension+diabetes+alcoholism+handcap,
               data = undertrain,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full')


plot(n)
head(undertrain)

#  Prediction on Train set
output_train <- neuralnet::compute(n, undertrain[,2:10])
head(output_train$net.result)
head(undertrain[1,])

p1 <- output_train$net.result
pred1 <- ifelse(p1>0.5,1,0)
head(pred1)
tab1 <- table(pred1,undertrain$no_show)
tab1
miscrate <- 1-sum(diag(tab1))/sum(tab1)
cat('Misclassification rate on Train set:',round(miscrate*100,2),'%')


#  Prediction on Test set
output_test <- neuralnet::compute(n, undertest[,2:10])
head(output_test$net.result)
head(undertest[1,])

p2 <- output_test$net.result
pred2 <- ifelse(p2>0.5,1,0)
head(pred2)
tab2 <- table(pred2,undertest$no_show)
tab2
miscrate_test <- 1-sum(diag(tab2))/sum(tab2)
cat('Misclassification rate on Test set:',round(miscrate_test*100,2),'%')
cat('Model Results:')
cat('Misclassification rate on Train set and Test Set are:',round(miscrate*100,2),'%',round(miscrate_test*100,2),'%')

#head(undertest)
result<- cbind(undertest,pred2)
#head(result)
result <- result[,c((2:10),1,11)]

colnames(result)[colnames(result)=="pred2"] <- "predicted"
View(result)


