#loading libraries
library (ISLR )
library(caret)
library(dplyr)

#reading the file
df <- read.csv("/Users/rajivranjan/Desktop/slump.csv", header=TRUE)
df<-select(df, Cement,Slag,Fly.ash,Water,SP,Coarse.Aggr.,Fine.Aggr.,FLOW.cm.)

#randomly selecting 85 values
smp_size <- floor(0.83 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

#selecting test and train values
train <- df[train_ind, ]
test <- df[-train_ind, ]

#creating a model of  degree 1
Control1<-trainControl(method = "repeatedcv",repeats = 5)
LmFit<-train(FLOW.cm. ~ ., data = train, method = "lm",trControl = Control1, metric="Rsquared")
PredictedTest<-predict(LmFit,test)
ModelTest<-data.frame(obs = test$FLOW.cm., pred=PredictedTest)
defaultSummary(ModelTest)
plot(test$FLOW.cm.,PredictedTest,xlab="actual",ylab="predicted",col = "red")


#creating a model of plynomial degree 2
LmFit2<- lm(FLOW.cm. ~ poly(Cement, degree=2, raw=TRUE) +
     poly(Slag, degree=2, raw=TRUE) +
     poly(Fly.ash, degree=2, raw=TRUE) +
     poly(Water, degree=2, raw=TRUE) +
     poly(SP, degree=2, raw=TRUE) +
     poly(Coarse.Aggr., degree=2, raw=TRUE) +
     poly(Fine.Aggr., degree=2, raw=TRUE) ,data=train)
PredictedTest2<-predict(LmFit2,test)
ModelTest2<-data.frame(obs = test$FLOW.cm., pred=PredictedTest2)
defaultSummary(ModelTest2)
plot(test$FLOW.cm.,PredictedTest2,xlab="actual",ylab="predicted",col = "red")


#creating a model of plynomial degree 3
LmFit3<- lm(FLOW.cm. ~ poly(Cement, degree=3, raw=TRUE) +
              poly(Slag, degree=3, raw=TRUE) +
              poly(Fly.ash, degree=3, raw=TRUE) +
              poly(Water, degree=3, raw=TRUE) +
              poly(SP, degree=3, raw=TRUE) +
              poly(Coarse.Aggr., degree=3, raw=TRUE) +
              poly(Fine.Aggr., degree=3, raw=TRUE) ,data=train)
PredictedTest3<-predict(LmFit3,test)
ModelTest3<-data.frame(obs = test$FLOW.cm., pred=PredictedTest3)
defaultSummary(ModelTest3)
plot(test$FLOW.cm.,PredictedTest3,xlab="actual",ylab="predicted",col = "red")


#creating a model of plynomial degree 4
LmFit4<- lm(FLOW.cm. ~ poly(Cement, degree=4, raw=TRUE) +
              poly(Slag, degree=4, raw=TRUE) +
              poly(Fly.ash, degree=4, raw=TRUE) +
              poly(Water, degree=4, raw=TRUE) +
              poly(SP, degree=4, raw=TRUE) +
              poly(Coarse.Aggr., degree=4, raw=TRUE) +
              poly(Fine.Aggr., degree=4, raw=TRUE) ,data=train)
PredictedTest4<-predict(LmFit4,test)
ModelTest4<-data.frame(obs = test$FLOW.cm., pred=PredictedTest4)
defaultSummary(ModelTest4)
plot(test$FLOW.cm.,PredictedTest4,xlab="actual",ylab="predicted",col = "red")


#creating a model of plynomial degree 5
LmFit5<- lm(FLOW.cm. ~ poly(Cement, degree=5, raw=TRUE) +
              poly(Slag, degree=5, raw=TRUE) +
              poly(Fly.ash, degree=5, raw=TRUE) +
              poly(Water, degree=5, raw=TRUE) +
              poly(SP, degree=5, raw=TRUE) +
              poly(Coarse.Aggr., degree=5, raw=TRUE) +
              poly(Fine.Aggr., degree=5, raw=TRUE) ,data=train)
PredictedTest5<-predict(LmFit5,test)
ModelTest5<-data.frame(obs = test$FLOW.cm., pred=PredictedTest5)
defaultSummary(ModelTest5)
plot(test$FLOW.cm.,PredictedTest5,xlab="actual",ylab="predicted",col = "red")



