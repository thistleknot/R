source("RMSE BackStep CV.R")
source("functions.R")

#knn.reg.bestK(x_training, x_test, y_training, y_test)

#normalization (not being used)
if(normalizeResponse=="Y")
{
  #trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  trainParam <- caret::preProcess(as.matrix(set.train),method=c("center", "scale"))
  
  set.train <- predict(trainParam, data2)
  
  set.test <- predict(trainParam, data2)
}

set.train2 <- set.train

dataset_knn <- back_step_partial_correlation_knn(set.train2)
dataset_reduced <- rmse_backstep_knn(dataset_knn)

#dataset_reduced <- rmse_backstep_knn(set.train2)
knn_predictors <- read.csv(text=paste (dataset_reduced[[2]][[1]], collapse=","),header = FALSE)

#knn_predictors <- colnames(dataset_knn)

set.train2 <- set.train[,c(unlist(knn_predictors))]

corPlot(PCOR(set.train2))
knn_model <- knn.reg.bestK(set.train2)

#knn_model$k.opt
tested1 <- set.test[,1]
tested2 <- rbind(set.train,set.test)[,1]
#predict on test data
predictions1 = knn.reg(set.train[,-1,drop=FALSE], test=set.test[,colnames(set.train)][,-1,drop=FALSE], unlist(set.train[,1,drop=FALSE]), knn_model$k.opt)
predictions2 = knn.reg(rbind(set.train,set.test)[,colnames(set.train2)][,-1,drop=FALSE], test=rbind(set.train,set.test)[,colnames(set.train2)][,-1,drop=FALSE], unlist(rbind(set.train,set.test)[,colnames(set.train2)][,1,drop=FALSE]), knn_model$k.opt)
#yhat$R2Pred

if(normalizeResponse=="Y")
{
  predictions1 <- (predictions1$pred * trainParam$std[1]) + trainParam$mean[1]
  predictions2 <- (predictions2$pred * trainParam$std[1]) + trainParam$mean[1]
  tested1 <- (tested1 * trainParam$std[1]) + trainParam$mean[1]
  tested2 <- (tested2 * trainParam$std[1]) + trainParam$mean[1]
}

#predict on self data with best knn
RMSE(predictions1,unlist(tested1))
MAPE(predictions1,unlist(tested1))
plot(predictions1,unlist(tested1))

RMSE(predictions2,unlist(tested2))
MAPE(predictions2,unlist(tested2))
plot(predictions2,unlist(tested2))

#resid <- unlist(set.final[,1])-yhatFull$pred

library(dbscan)
kNNdistplot(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt)

eps <- median(kNNdist(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt))

cl <- dbscan(rbind(set.train,set.test)[,colnames(set.train2)], eps = eps, minPts = knn_model$k.opt)
pairs(rbind(set.train,set.test)[,colnames(set.train2)], col = cl$cluster+1L)
