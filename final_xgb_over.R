# Packages
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ROSE)
library(lattice)
library(ggplot2)
library(caret)
library(car)
# Data
data <- read.csv('ABCD_states.csv', header = T)

data <- data[,c(12,1,2,3,4,5,6,7,8,9,10,11)]
head(data)

data$state <- as.factor(data$state)
#data$no_show <- as.factor(data$no_show)
data$gender <- as.factor(data$gender)
data$scholarship <- as.factor(data$scholarship)
data$hipertension <- as.factor(data$hipertension)
data$diabetes <- as.factor(data$diabetes)
data$alcoholism <- as.factor(data$alcoholism)
data$sms_received <- as.factor(data$sms_received)
data$handcap <- as.factor(data$handcap)

data$age <- cut(data$age, breaks=c(0,18,65,99), labels=c(1,2,3))
data$age <- as.factor(data$age)
str(data)

# Partition data
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

table(train$no_show)
overtrain <- ovun.sample(no_show~., data=train, method='over', N= 136254)$data
table(overtrain$no_show)

table(test$no_show)
overtest <- ovun.sample(no_show~., data=test, method='over', N= 34028)$data
table(overtest$no_show)

View(overtrain)

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(no_show ~ .-1, data = overtrain)
head(trainm)
train_label <- overtrain[,"no_show"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
testm <- sparse.model.matrix(no_show~.-1, data = overtest)
test_label <- overtest[,"no_show"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
nc
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 300,
                       watchlist = watchlist,
                       eta = 0.001,
                       max.depth = 3,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 333)

# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

#min(e$test_mlogloss)
#e[e$test_mlogloss == 0.625217,]

# Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix)

pred <- matrix(p, nrow = nc, ncol = length(p)/(nc)) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
View(pred)


t <- table(Prediction = pred$max_prob, Actual = pred$label)
print(t)
x  <-table(pred$max_prob,pred$label)
percent_accuracy <- round((x['0','0'] + x['1','1'])/(x['0','0'] + x['0','1'] + x['1','0'] + x['1','1'])*100,2)
cat('Model Accuracy is:',percent_accuracy,'%')
cat('Misclassification Rate is:',100-percent_accuracy,'%')


overtest <- overtest[,c((2:12),1)]
final_df <- cbind(overtest, pred$max_prob)
colnames(final_df)[13] <- 'predicted'
#final_df$neighbourhood <- NULL

final_df$gender= ifelse(final_df$gender==1,'Male','Female')
final_df$scholarship= ifelse(final_df$scholarship==1,'Yes','No')
final_df$hipertension= ifelse(final_df$hipertension==1,'Yes','No')
final_df$alcoholism= ifelse(final_df$alcoholism==1,'Yes','No')
final_df$diabetes= ifelse(final_df$diabetes==1,'Yes','No')
final_df$sms_received= ifelse(final_df$sms_received==1,'Yes','No')
final_df$no_show= ifelse(final_df$no_show==1,'Yes','No')
final_df$predicted= ifelse(final_df$predicted==1,'Yes','No')
final_df$handcap= ifelse(final_df$handcap==1,'Yes','No')

final_df$age=ifelse(final_df$age==1,'Under 18',ifelse(final_df$age==2,'Middle-aged','Senior Citizen'))


View(final_df)
write.csv(final_df,'XGB Predictions_New.csv', row.names = FALSE)
