library(MASS, quietly=TRUE)
library(caret)
library(ROSE)
library(ROCR)

data<-read.csv('ABCD_states.csv',header = T)
View(data)
dim(data)
names(data)
head(data, 12)
#colnames(data)[2] <- "diffdays"

#?replace

#replace(data['gender'],c(0,1),c("Female","Male"))


set.seed(1234)
data$state <- as.factor(data$state)
data$neighbourhood <- NULL

data$no_show <- as.factor(data$no_show)
data$gender <- as.factor(data$gender)
data$scholarship <- as.factor(data$scholarship)
data$hipertension <- as.factor(data$hipertension)
data$diabetes <- as.factor(data$diabetes)
data$alcoholism <- as.factor(data$alcoholism)
data$sms_received <- as.factor(data$sms_received)
data$handcap <- as.factor(data$handcap)


#data$age <- cut(data$age, breaks=c(0,18,65,99))
#data$age <- as.factor(data$age)


levels(data$gender) <- c('F','M')
data$gender <- as.factor(data$gender)
levels(data$no_show) <- c('Shows','NotShows')
levels(data$scholarship) <- c('No','Yes')
levels(data$hipertension) <- c('No','Yes')
levels(data$diabetes) <- c('No','Yes')
levels(data$alcoholism) <- c('No','Yes')
levels(data$sms_received) <- c('No','Yes')
levels(data$handcap) <- c('No','Yes')
#levels(data$age) <- c('U','M','S')

str(data)

ind <- createDataPartition(data$no_show, p=1/100, list = FALSE)
trainDF <- data[ind,]
testDF <- data[-ind,]
table(trainDF$no_show)
table(testDF$no_show)

dim(testDF)

undertrainDF <- ovun.sample(no_show~., data=trainDF, method='both')$data
table(undertrainDF$no_show)

undertestDF <- ovun.sample(no_show~., data=testDF, method='both')$data
table(undertestDF$no_show)


ControlParametrs <- trainControl(method = 'cv', 
                                 number = 5, 
                                 savePredictions = TRUE,
                                 classProbs=TRUE)

paramtersGrid <- expand.grid(mtry=c(1:21))

dfnames <- names(data)
dfnames

#make.names(dfnames, unique = FALSE, allow_ = TRUE)
modelRandom <- train(no_show ~.,
                     data = undertrainDF,
                     method='rf',
                     trControl = ControlParametrs,
                     tuneGrid = paramtersGrid
                     )
preds_zero_one=predict(modelRandom,undertestDF)
#preds_zero_one <- as.data.frame(ifelse(preds=='Shows',0,1))
class(preds_zero_one)

pred_df=cbind(undertestDF,preds_zero_one)
colnames(pred_df)[12] <- "predicted"
pred_df <- pred_df[sample(nrow(pred_df)),]
View(pred_df)
tab <- table(predicted=pred_df$predicted, Actual=pred_df$no_show)
tab
acc=(sum(diag(tab))/sum(tab))
sens=sum(tab[])
cat('Model Accuracy:',acc)
plot(modelRandom,xlab='Values of MTRY',ylab='Cross Validation Accuracy')
modelRandom

cm<-confusionMatrix(pred_df$no_show, pred_df$predicted, positive = 'NotShows')
performance_metrics <- round(cm$byClass*100,2) 
performance_metrics
