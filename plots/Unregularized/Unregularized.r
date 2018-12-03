#loading libraries
library (ISLR )
library(caret)
library(dplyr)
library(DAAG)
library(glmnet)


#reading the input file
df <- read.csv("/Users/rajivranjan/Desktop/slump.csv", header=TRUE)
df<-select(df, Cement,Slag,Fly.ash,Water,SP,Coarse.Aggr.,Fine.Aggr.,FLOW.cm.)

#randomly selecting 85 Samples
smp_size <- floor(0.83 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

#divinding train and test data
train <- df[train_ind, ]
test <- df[-train_ind, ]

#generating the model
Control1<-trainControl(method = "repeatedcv",number = 5,repeats=5)
LmFit2<-train(FLOW.cm. ~ ., data = train, method = "glm",trControl = Control1, metric="Rsquared")
summary(LmFit2)

#using the model to predict the values
PredictedTest2<-predict(LmFit2,test)
ModelTest2<-data.frame(obs = test$FLOW.cm., pred=PredictedTest2)
defaultSummary(ModelTest2)

#calculating the residuals
ResData3<-residuals(LmFit2)
PredictedValues2<-predict(LmFit2)
plot(train$FLOW.cm.,ResData3,xlab="data")
abline(0,0)

#plotting the actual vs predicted chart
plot(test$FLOW.cm.,PredictedTest2,xlab="actual",ylab="predicted",col = "red")

