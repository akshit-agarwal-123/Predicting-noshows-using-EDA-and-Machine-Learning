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


#*****************************************************************************************************************
################# RANDOM FOREST ###################################################################################



data <- read.csv('ABCD_states.csv')
View (data)

data$state=as.factor(data$state)
data$no_show <- as.factor(data$no_show)
data$gender=as.factor(data$gender)
data$scholarship <- as.factor(data$scholarship)
data$hipertension <- as.factor(data$hipertension)
data$diabetes <- as.factor(data$diabetes)
data$alcoholism <- as.factor(data$alcoholism)
data$sms_received <- as.factor(data$sms_received)
data$handcap <- as.factor(data$handcap)

class(data$age)
data$age <- cut(data$age, breaks=c(0,18,65,99), labels=c(1,2,3))
data$age <- as.factor(data$age)
str(data)


set.seed(123)
ind <- sample(2,nrow(data),replace=TRUE, prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]
View(training)
class(training$no_show)
class(testing$no_show)
dim(training)
dim(testing)



library(randomForest)
set.seed(123)

# Random forest without oversampling does horribly bad performance
rf <- randomForest(no_show~difference+state+sms_received+age+gender, data=training,ntree=400,mtry=3.464)
print(rf)
attributes(rf)
confusionMatrix(predict(rf, testing),testing$no_show,positive='1')
#table(training$no_show)

# Random forest with oversampling
over <- ovun.sample(no_show~., data=training, method="over", N=135604)$data
table(over$no_show)
summary(over)
rfover <- randomForest(no_show~difference+state+sms_received+age+gender, data=over,ntree=700,mtry=3.464)
confusionMatrix(predict(rfover, testing),testing$no_show,positive='1')
summary(rfover)

p1<-predict(rf,training)
confusionMatrix(p1,training$no_show)

p2<-predict(rf,testing)
confusionMatrix(p2,testing$no_show)

plot(rf)
varImpPlot(rfover)


#x <-cbind(training[,1:3],training["state"],training["sms_received"],training[,6:10])
x <-cbind(over['difference'],over["state"],over["sms_received"],over['age'],over['gender'])

View(x)


y <- over['no_show']

str(over)
#y$no_show <- as.factor(y$no_show)

y$no_show=as.character(y$no_show)
y$no_show=as.factor(y$no_show)
View(y)
str(y)

""" 
t <- tuneRF(x,y,
            stepFactor = 2, 
            plot = TRUE, 
            ntreeTry = 400, 
            trace = TRUE, 
            improve = 0.1,
            doBest = TRUE)

"""
View(training)
sqrt(ncol(training))
partialPlot(rf, training, difference, "0")
t <- tuneRF(x,over[,12],
            stepFactor =3.5, 
            plot = TRUE,
            mtryStart = sqrt(ncol(training)),
            ntreeTry = 800, 
            trace = TRUE, 
            improve = 1e-5,
            doBest = TRUE
            )

hist(treesize(rf),col='green')

class(t)

