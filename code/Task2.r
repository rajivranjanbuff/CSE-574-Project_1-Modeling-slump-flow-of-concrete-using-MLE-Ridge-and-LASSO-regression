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
LassoMod <- cv.glmnet(x1, y1, alpha=1, nlambda=100,lambda.min.ratio=0.0001,nfolds=5)


#Plotting the best model Ridge
fit1 <- RidgeMod$glmnet.fit

plot(fit1,xlab="L2 Norm",main="Degree of freedom")
grid (NULL,NULL, lty = 1, col = "brown4") 

legend("topleft", legend=c("Cement", "Slag","Fly.ash","Water","SP","Coarse.Aggr.","Fine.Aggr."),
       col=c("gray74", "red","green","blue","paleturquoise","orchid","black"), lty=1, cex=0.8,
       text.font=2)
#plotting the best model Lasso

fit2 <- LassoMod$glmnet.fit

plot(fit2,xlab="L1 Norm",main="Degree of freedom")
grid (NULL,NULL, lty = 1, col = "brown4") 

legend("topleft", legend=c("Cement", "Slag","Fly.ash","Water","SP","Coarse.Aggr.","Fine.Aggr."),
       col=c("gray74", "red","green","blue","paleturquoise","orchid","black"), lty=1, cex=0.8,
        text.font=2)






