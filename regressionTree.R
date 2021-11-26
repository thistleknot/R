library(whitening)
library(rpart)
library(rpart.plot)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)
PCA=FALSE
whiten=TRUE

normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]

rownames(data2) <- data[,1]

nr <- nrow(data2)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

#normalization (not being used)
if(normalizeResponse=="Y")
{
  #trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  trainParam <- caret::preProcess(as.matrix(set.train),method=c("center", "scale"))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

set.train.X <- set.train[,-1]
set.train.Y <- set.train[,1,drop=FALSE]

set.text.X <- set.test[,-1]
set.text.Y <- set.test[,1,drop=FALSE]

#normalization (not being used)
if(normalizeResponse=="Y")
{
  #trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  
  trainParam <- caret::preProcess(as.matrix(set.train.X),method=c("center", "scale"))
  
  set <- predict(trainParam, set.train.X)
  
  if(PCA)
  {
    #set <- whiten(as.matrix(set))
    set <- whiten(as.matrix(set),method=c("ZCA"),center=TRUE)
    set.pca <- prcomp(set, center=FALSE, scale=FALSE)
  }
  #decorrelate (i.e. whiten)
  
  if(whiten)
  {
    set <- whiten(as.matrix(set),method=c("ZCA"),center=TRUE)
  }
  colnames(set) <- colnames(set.train.X)
  
  
}


best.dims = ncol(data2)

## 3. Regression Tree ##
fit = rpart(Poverty~., method="anova", data=data.frame(set))
par(mfrow=c(1,1))
rpart.plot(fit, roundint=FALSE)

#drop 1st column from prediction x's
yhat.test = predict(fit, newdata = dat[id.test,-1])
rmse(yhat.test, ytest)