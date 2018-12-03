#loading libraries
library (ISLR )
library(caret)
library(dplyr)
library(glmnet)
library(elasticnet)
library(DAAG)

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
LassoMod <- cv.glmnet(x1, y1, alpha=1, nlambda=100,lambda.min.ratio=0.0001,nfolds=5)
Control1<-trainControl(method = "repeatedcv",number = 5,repeats=5)
LmFit2<-train(FLOW.cm. ~ ., data = train1, method = "glm",trControl = Control1, metric="Rsquared")

#using the model to predict the values
y_predicted1 <- predict(RidgeMod, newdata=test1,newx=xtest1,s="lambda.min")
y_predicted4 <- predict(LassoMod, newdata=test1,newx=xtest1,s="lambda.min")
PredictedTest2<-predict(LmFit2,test1)

#plotting the actual vs predicted chart
plot(test1$FLOW.cm.,y_predicted1,xlab="actual",ylab="predicted",col = "red")
legend("topleft", legend=c("Ridge L2", "Lasso L1", "MLE"), col=c("red", "blue","green"),pch=c(1,1,1),cex=0.8,title="Regularizations", text.font=4, bg='azure')
par(new=TRUE)
plot(test1$FLOW.cm.,y_predicted4,xlab="",ylab="",col = "blue",axes=FALSE)
par(new=TRUE)
plot(test1$FLOW.cm.,PredictedTest2,xlab="",ylab="",col = "green",axes=FALSE)




