rm(list = ls())


is.exist <- "ggplot2" %in% rownames(installed.packages())
if(!is.exist){install.packages("ggplot2")}
is.exist <- "FactoMineR" %in% rownames(installed.packages())
if(!is.exist){install.packages("FactoMineR")}
is.exist <- "factoextra" %in% rownames(installed.packages())
if(!is.exist){install.packages("factoextra")}

library(nnet)
library(ggplot2)
library(FactoMineR)
library(factoextra)

memory.limit(1024^4)
options(scipen = 5)

## Import data
var<-read.csv("MLR.csv",header = T,stringsAsFactors = T)
var[,2:63]<-lapply(var[,2:63] , factor)

##MCA: Multiple corresponding analysis
mca <- MCA(var[,-1],graph = F)
scores <- mca$ind$coord

set.seed(100)
trainingRows <- sample(1:nrow(scores), 0.7*nrow(scores))
trainingdata <- var[trainingRows, ]
testdata <- var[-trainingRows, ]

##MNL regression: Multinormial Logit Regression
model<-multinom(Y ~.,data = trainingdata,maxit= 1000,MaxNWts =10000000)

## Testing
predict <- predict(model,testdata)
probs <- sum(ifelse(predict == testdata[,1],1,0))/dim(testdata)[1]