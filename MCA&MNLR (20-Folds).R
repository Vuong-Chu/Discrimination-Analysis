rm(list = ls())


is.exist <- "ggplot2" %in% rownames(installed.packages())
if(!is.exist){install.packages("ggplot2")}
is.exist <- "FactoMineR" %in% rownames(installed.packages())
if(!is.exist){install.packages("FactoMineR")}
is.exist <- "factoextra" %in% rownames(installed.packages())
if(!is.exist){install.packages("factoextra")}
is.exist <- "polycor" %in% rownames(installed.packages())
if(!is.exist){install.packages("polycor",dependencies= TRUE)}
is.exist <- "raster" %in% rownames(installed.packages())
if(!is.exist){install.packages("raster",dependencies= TRUE)}
is.exist <- "usdm" %in% rownames(installed.packages())
if(!is.exist){install.packages("usdm",dependencies= TRUE)}
is.exist <- "psych" %in% rownames(installed.packages())
if(!is.exist){install.packages("psych",dependencies= TRUE)}

library(nnet)
library(sp)
library(raster)
library(usdm)
library(psych)
library(polycor)

memory.limit(1024^4)
options(scipen = 5)

var<-read.csv("MLR.csv",header = T,stringsAsFactors = T)

var[,2:63]<-lapply(var[,2:63] , factor)

##Polychor

het.mat<-hetcor(var[,-1])$cor

rmcl = function(cor_mat, threshold) {
  cor_mat = abs(cor_mat)
  stopifnot(sum(cor_mat[is.na(cor_mat)]) == 0) 
  for (i in 1:(nrow(cor_mat) - 1)) {
    for (j in (i+1):ncol(cor_mat)) {
      if(cor_mat[i, j] > threshold) {
        cor_mat[i, ] = rep(NA, ncol(cor_mat))
        break
      }
    }
  }
  idx = which(!is.na(cor_mat[, 1]))
  cor_mat[idx, idx]
}

Result <- matrix(0,ncol=11,nrow=20)

u<-1
for (i in seq(0.5, 1, by = 0.05)){
  adopt_cor <- rmcl(het.mat,i)
  var1<-var[,c("Y",c(unlist((dimnames(adopt_cor)[1]))))]
  folds <- cut(seq(1,nrow(var1)),breaks=20,labels=FALSE)
  
  for(k in 1:20){
    testIndexes <- which(folds==k,arr.ind=TRUE)
    testingdata <- var1[testIndexes, ]
    trainingdata <- var1[-testIndexes, ]
    ##MNL regression: Multinormial Logit Regression
    model<-multinom(Y ~.,data = trainingdata,maxit= 1000,MaxNWts =10000000)
    
    ## Testing
    predict <- predict(model,testingdata)
    probs <- sum(ifelse(predict == testingdata[,1],1,0))/dim(testingdata)[1]
    Result[k,u]<-probs            
                }
    u <- u + 1
                  }
col.names(Result)<-paste("Threshold",seq(0.5, 1, by= 0.05))

