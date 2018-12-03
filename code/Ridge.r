#loading libraries
library (ISLR )
library(caret)
library(dplyr)
library(glmnet)
library(elasticnet)

#reading the input file
df1 <- read.csv("/Users/rajivranjan/Desktop/slump.csv", header=TRUE)
df1<-select(df1, Cement,Slag,Fly.ash,Water,SP,Coarse.Aggr.,Fine.Aggr.,FLOW.cm.)

#randomly selecting 85 Samples
smp_size1 <- floor(0.83 * nrow(df1))
set.seed(123)
train_ind1 <- sample(seq_len(nrow(df1)), size = smp_size1)

#divinding train and test data
train1 <- df1[train_ind1, ]
test1 <- df1[-train_ind1, ]

#changing the data set into matrix bcz ridgefit takes only matrix
#selecting all the values except FLOW.cm in x
x1<-model.matrix(FLOW.cm.~.,train1)
y1<-as.matrix(train1$FLOW.cm.)

xtest1<-model.matrix(FLOW.cm.~.,test1)
ytest1<-as.matrix(test1$FLOW.cm.)

#generating the model
RidgeMod <- cv.glmnet(x1, y1, alpha=0, nlambda=100,lambda.min.ratio=0.0001,nfolds=5)

#plotting the log(Lamda) vs the Mean Squared Error of the model
plot(RidgeMod,xvar="lambda",label=TRUE,xlab="log(Lambda) of L2 Ridge regularization")

#Best Lambda
best.lambda1 <- RidgeMod$lambda.min
best.lambda1

#getting the coefficients for the best model
predict(RidgeMod, s=best.lambda1, type="coefficients")

#Plotting the best model 
fit1 <- RidgeMod$glmnet.fit
plot(fit1,xlab="L2 Norm",main="Degree of freedom")
plot(RidgeMod)

#using the model to predict the values
y_predicted1 <- predict(RidgeMod, newdata=test1,newx=xtest1,s="lambda.min")
ModelTest3<-data.frame(obs=test1$FLOW.cm., pred = y_predicted1)
ModelTest3 <- rename(ModelTest3, pred = X1)
defaultSummary(ModelTest3)
#plotting the actual vs predicted chart
plot(test1$FLOW.cm.,y_predicted1,xlab="actual",ylab="predicted",col = "red")


#Method 2
Control1<-trainControl(method = "cv",number=5)
ridgeFit <- train(FLOW.cm. ~ ., data = train1,method = 'ridge',preProc = c("center", "scale"),trControl = Control1, metric = "Rsquared")
PredictedTest2<-predict(ridgeFit,newdata = test1)
ModelTest2<-data.frame(obs = test1$FLOW.cm., pred=PredictedTest2)
defaultSummary(ModelTest2)
plot(test1$FLOW.cm.,PredictedTest2,xlab="predicted",ylab="actual",col = "red")
plot(ridgeFit)

#calculating the residuals
ResData<-residuals(ridgeFit)
PredictedValues2<-predict(ridgeFit)
plot(train1$FLOW.cm.,ResData,xlab="data")
abline(0,0)








