#loading libraries
library (ISLR )
library(caret)
library(dplyr)
library(glmnet)
library(elasticnet)

#reading the input file
df2 <- read.csv("/Users/rajivranjan/Desktop/slump.csv", header=TRUE)
df2<-select(df2, Cement,Slag,Fly.ash,Water,SP,Coarse.Aggr.,Fine.Aggr.,FLOW.cm.)

#randomly selecting 85 Samples
smp_size2 <- floor(0.83 * nrow(df2))
set.seed(123)
train_ind2 <- sample(seq_len(nrow(df2)), size = smp_size2)

#divinding train and test data
train2 <- df2[train_ind2, ]
test2 <- df2[-train_ind2, ]

#changing the data set into matrix bcz ridgefit takes only matrix
#selecting all the values except FLOW.cm in x
x2<-model.matrix(FLOW.cm.~.,train2)
y2<-as.matrix(train2$FLOW.cm.)

xtest2<-model.matrix(FLOW.cm.~.,test2)
ytest2<-as.matrix(test2$FLOW.cm.)

#generating the model
LassoMod <- cv.glmnet(x2, y2, alpha=1, nlambda=100,lambda.min.ratio=0.0001,nfolds=5)

#plotting the log(Lamda) vs the Mean Squared Error of the model
plot(LassoMod,xvar="lambda",label=TRUE,xlab="log(Lambda) of L1 Lasso regularization")

#Best Lambda
best.lambda2 <- LassoMod$lambda.min
best.lambda2

#getting the coefficients for the best model
predict(LassoMod, s=best.lambda2, type="coefficients")

#Plotting the best model 
fit2 <- LassoMod$glmnet.fit
plot(fit2,xlab="L1 Norm",main="Degree of freedom")
plot(LassoMod)

#using the model to predict the values
y_predicted4 <- predict(LassoMod, newdata=test2,newx=xtest2,s="lambda.min")
ModelTest5<-data.frame(obs=test2$FLOW.cm., pred = y_predicted4)
ModelTest5 <- rename(ModelTest5, pred = X1)
defaultSummary(ModelTest5)
plot(test2$FLOW.cm.,y_predicted4,xlab="actual",ylab="predicted",col = "blue")


#Method 2
Control3<-trainControl(method = "cv",number=5)
LassoFit <- train(FLOW.cm. ~ ., data = train2,method = 'ridge',preProc = c("center", "scale"),trControl = Control3, metric = "Rsquared")
PredictedTest4<-predict(LassoFit,newdata = test2)
ModelTest6<-data.frame(obs = test2$FLOW.cm., pred=PredictedTest4)
defaultSummary(ModelTest6)
plot(test2$FLOW.cm.,PredictedTest4,xlab="actual",ylab="predicted",col = "blue")
plot(LassoFit)

#calculating the residuals
ResData2<-residuals(LassoFit)
PredictedValues4<-predict(LassoFit)
plot(train2$FLOW.cm.,ResData2,xlab="data")
abline(0,0)








