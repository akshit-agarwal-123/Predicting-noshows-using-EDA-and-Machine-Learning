library(Rcpp)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(ISLR)


#Accuracy: 62%-68%, Mode=66%

dta <- read.csv('ABCD_states.csv')
#print(dta)
str(dta)
cor(dta$difference,dta$no_show,method="pearson")
cor(dta$gender,dta$no_show,method="pearson")
cor(dta$alcoholism,dta$no_show,method="pearson")
cor(dta$diabetes,dta$no_show,method="pearson")
cor(dta$sms_received,dta$no_show, method='pearson')
#print(dta$gender)
dta$gender=as.factor(dta$gender)


dta$scholarship <- as.factor(dta$scholarship)
dta$hipertension <- as.factor(dta$hipertension)
dta$diabetes <- as.factor(dta$diabetes)
dta$alcoholism <- as.factor(dta$alcoholism)
dta$sms_received <- as.factor(dta$sms_received)
dta$handcap <- as.factor(dta$handcap)
dta$no_show <- as.factor(dta$no_show)
str(dta)
#View(dta)


dta$age <- cut(dta$age, breaks=c(0,18,65,99), labels=c(1,2,3))
dta$age <- as.factor(dta$age)


head(dta)
ncol(dta)
str(dta)
dim(dta)

set.seed(123)
ind <- sample(2,nrow(dta),replace=TRUE, prob = c(0.7,0.3))
training <- dta[ind==1,]
testing <- dta[ind==2,]
class(training$no_show)
table(training$no_show)
str(testing)
str(testing$no_show)

library(party)

table(training$no_show)


over <- ovun.sample(no_show~., data=training, method="over", N=119450)$data
table(over$no_show)
summary(over)


tree1 <- ctree(no_show~difference+sms_received+alcoholism+age+gender,data=over,controls = ctree_control(teststat ='max',testtype = 'Teststatis' ,mincriterion=0.65,minsplit = 20000, minbucket = 7000))
plot(tree1)
class(tree1)
preds=(predict(tree1,testing,type='prob'))
head(preds)



#print(preds[[1]][2])
#pred_data=data.frame(preds[1:10])
#print(pred_data)
#print(pred_data[1])
#class(pred_data)
#ncol(pred_data)
#preds=as.data.frame(preds)
#class(preds)

#class(preds)

#Convert to a dataframe, transpose, and convert the resulting matrix back to a dataframe
preds= as.data.frame(t(as.data.frame(preds)))
preds<-round(preds,3) 

head(preds)
rownames(preds) <- NULL

print(preds)
preds[2] <- NULL
head(preds)

preds[1]
preds[1] <- ifelse(preds[1]>=0.5,0,1)
print(preds[1])
class(preds[1])

testing_no_show <- as.data.frame(testing$no_show)
class(testing_no_show)
testing_no_show
head(testing_no_show)

dim(preds[1])
nrow(preds[1])


final_df=cbind(preds[1],testing$no_show)
final_df
tab <- table(final_df)

print('confusion matrix')
print(tab)

cat('% of accuracy:',(sum(diag(tab))/sum(tab)),'\n')
cat('% of misclassifcation:',(1-(sum(diag(tab))/sum(tab))))

