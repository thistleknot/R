library(caret)
library(leaps)
library(tidyverse)
library(car)
library(whiten)

normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

nr <- nrow(data2)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

#normalization (not being used)
if(normalizeResponse=="Y")
{
  trainParam <- caret::preProcess(as.matrix(set.train))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/#:~:text=The%20best%20subsets%20regression%20is,best%20subsets%20regression%20using%20R.
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

models <- regsubsets(as.formula(paste0(colnames(set.train)[1], "~.")), data = set.train, nvmax = ncol(set.train)-1)

res.sum <- summary(models)

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_cv_error <- function(model.formula, data){
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
# Compute cross-validation error

#get_model_formula(3, models, "Poverty")

model.ids <- 1:(ncol(set.train)-1)

cv.errors <-  map(model.ids, get_model_formula, models, "Poverty") %>%
  map(get_cv_error, data = data) %>%
  unlist()

cv.errors

coef(models, which.min(cv.errors))

finalNames <- names(coef(models, which.min(cv.errors)))[-1]

f <- as.formula(
  paste(colnames(set.train[1]), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,set.train)
summary(model)

tested <- set.test[,1]
predictions <- predict(model,set.test)

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

wd <- cbind(scale(data2[,1,drop=FALSE]),whiten(as.matrix(data2[,-1][finalNames]), method=c("ZCA")))

finalModel <- lm(wd)
summary(finalModel)

hist(finalModel$residuals)

par(mfrow = c(2, 3))

plot(model,1)
plot(model,2)
plot(model,3)
plot(model,4)
#plot(model,5)
plot(hatvalues(finalModel),studres(finalModel))
#plot(cooks.distance(finalModel),studres(finalModel))
plot(model,6)

leveragePlots(finalModel)
