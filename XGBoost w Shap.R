library(xgboost)
library(coefplot)

options(digits = 3)

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

normalizeResponse <- "Y"

nr <- nrow(data2)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

if(normalizeResponse=="Y")
{
  trainParam <- caret::preProcess(as.matrix(set.train))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

best_param = list()
#best_seednumber = 1234
best_loss = Inf
best_loss_index = 0

cv.nround = 20
cv.nfold = 10

for (iter in 1:100) {
  param <- list(
                alpha = sample(0:10, 1)/10,
                lambda = sample(0:10, 1)/10
  )
  
  #https://stackoverflow.com/questions/35050846/xgboost-in-r-how-does-xgb-cv-pass-the-optimal-parameters-into-xgb-train
  mdcv <- xgb.cv(data = as.matrix(set.train[,-1]), params = param, nthread=4, nfold=cv.nfold,label = as.matrix(set.train[,1,drop=FALSE]),booster = "gblinear", objective = "reg:squarederror", max.depth = 10, nround = cv.nround, lambda_bias = 0)
  
  min_loss = min(mdcv$evaluation_log$test_rmse_mean)
  min_loss_index = which(mdcv$evaluation_log$test_rmse_mean==min_loss)
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    #best_seednumber = 
    best_param = param
  }
}

nround = best_loss_index
set.seed(best_seednumber)

param <- list(
           booster = "gblinear", 
           objective = "reg:squarederror", 
           max.depth = 10, 
           eval_metric='rmse',
           lambda = best_param$lambda, 
           lambda_bias = 0,
           alpha = best_param$alpha,
           nthread=4)

model <- xgb.train(param, xgb.DMatrix(as.matrix(set.train[,-1]),label = as.matrix(set.train[,1,drop=FALSE])),nrounds = cv.nround ,callbacks = list(cb.gblinear.history()))

model$niter

#https://www.rdocumentation.org/packages/coefplot/versions/1.2.6/topics/extract.coef.xgb.Booster
extract.coef(model)
xgb.gblinear.history(model, class_index = NULL)

predictions <- (rowSums(t(extract.coef(model)[,1]) * set.test[,-1])* trainParam$std[1]) + trainParam$mean[1]

predictions <- (rowSums(t(extract.coef(model)[,1]) * set.test[,-1])* trainParam$std[1]) + trainParam$mean[1]

tested <- set.test[,1]
tested <- (tested * trainParam$std[1]) + trainParam$mean[1]

RMSE(predictions,tested)
MAPE(tested,predictions)

plot(tested,predictions)
abline(lm(tested~predictions))
cor(tested,predictions)



mat <- xgb.importance (feature_names = colnames(set.train[,-1]),model = model)
xgb.plot.importance (importance_matrix = mat[1:ncol(set.train[,-1])]) 

source("shap.R")
shap_result = shap.score.rank(xgb_model = model, 
                              X_train =as.matrix(set.train[,-1]),
                              shap_approx = F
)

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

## Plot var importance based on SHAP
var_importance(shap_result, top_n=ncol(set.train[,-1]))

## Prepare data for top N variables
shap_long = shap.prep(shap = shap_result,
                      X_train = as.matrix(set.train[,-1]) , 
                      top_n = ncol(set.train[,-1])
)

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)

library(popbio)
mean.list(xgb.gblinear.history(model, class_index = NULL))[min_loss_index,]

## 
xgb.plot.shap(data = as.matrix(set.train[,-1]), # input data
              model = model, # xgboost model
              features = names(shap_result$mean_shap_score[1:ncol(set.train[,-1])]), # only top 10 var
              n_col = 3, # layout option
              plot_loess = T # add red line to plot
)

predictions  <- predict(model, as.matrix(set.test[,-1]))

fitted <- model$fitted.values
trained <- set.train[,1] 
tested <- set.test[,1]

if(normalizeResponse=="Y")
{
  fitted <- (fitted * trainParam$std[1]) + trainParam$mean[1]
  trained <- (trained * trainParam$std[1]) + trainParam$mean[1]
}

if(normalizeResponse=="Y")
{
  predictions <- (predictions * trainParam$std[1]) + trainParam$mean[1]
  tested <- (tested * trainParam$std[1]) + trainParam$mean[1]
}

RMSE(predictions,tested)
MAPE(tested,predictions)

plot(tested,predictions)
abline(lm(tested~predictions))
cor(tested,predictions)
