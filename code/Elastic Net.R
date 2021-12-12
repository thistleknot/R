library(matlib)

options(digits = 3)

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

nr <- nrow(data2)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

normalizeResponse <- "Y"

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

if(normalizeResponse=="Y")
{
        trainParam <- caret::preProcess(as.matrix(set.train))
        
        set.train <- predict(trainParam, set.train)
        
        set.test <- predict(trainParam, set.test)
}

numFolds = 10

folds=sample(rep(0:numFolds, length=nrow(set.train)))

#training portion (derive lambda's)
list.of.fits <- list()
for (i in 0:numFolds) {
        #i=1
        X <- as.matrix(set.train[folds!=i,-1,drop=FALSE])
        Y <- as.matrix(set.train[folds!=i,1,drop=FALSE])
        
        fit.name <- paste0("alpha", i/numFolds)
        
        list.of.fits[[fit.name]] <-
                cv.glmnet(X, Y, type.measure="mse", alpha=i/numFolds,
                          family="gaussian")
}

#View(list.of.fits)

results <- data.frame()

#validation portion, derive alpha's
for(i in 0:numFolds)
{#i=0
        objTest <- as.matrix(set.train[folds==i,])
        
        fit.name <- paste0("alpha", i/numFolds)
        
        predicted <- predict(list.of.fits[[fit.name]],
                             s=list.of.fits[[fit.name]]$lambda.1se, newx=objTest[,-1])
        
        mse <- mean((objTest[,1] - predicted)^2)
        
        temp <- data.frame(alpha=i/numFolds, mse=mse, fit.name=fit.name)
        results <- rbind(results, temp)
}

bestAlpha <- results$alpha[which(results$mse == min(results$mse))]

print(bestAlpha)

bestLambda <- cv.glmnet(X, Y, type.measure="mse", alpha=i/numFolds,
                        family="gaussian")$lambda.1se
print(bestLambda)

bestModel <- glmnet(X, Y, alpha = bestAlpha, lambda=bestLambda, standardize = FALSE, family = "gaussian", type.measure="deviance")

(coef(bestModel))
predictions <- predict.glmnet(bestModel, as.matrix(set.test[,-1]))

tested <- set.test[,1]

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




