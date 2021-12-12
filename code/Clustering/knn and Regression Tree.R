require(hydroGOF)
library(MASS)
library("corrplot")
library(leaps)
library(FNN)
#require(BCA)
#install.packages('party')
#install.packages('ROCR')
#library(fastDummies)
library(ROCR)
library(rpart)
library(caret)
library(rpart.plot)
require(rpart)
require(party)
#library(cluster)
#library(fpc)
library(dbscan)

pre_MyData <- read.csv(file="prepped.csv", header=TRUE, sep=",")

#fedvar was causing too much chaos with trainingDatasets having all 0's
dropColumns = colSums(pre_MyData == 0, na.rm = TRUE)

filtered <- c()
for (i in 1:nrow(data.frame(dropColumns)))
{
  #parsedList2 <- parsedList[!parsedList %in% c(filtered)]  
  
  #if(i>(floor*nrow(test1_z)))
  print(data.frame(dropColumns[i])[,1])
  #not really a percentage, more so a minimal acceptable loss, quarters are 75%, fedmin was 92%
  #if(data.frame(dropColumns[i])[,1]>=(seasonalConstant+2))
  if((data.frame(dropColumns[i])[,1])>=.80*nrow(pre_MyData))
  {
    print("yes")
    
    #works
    filtered <- rbind (filtered,i)
    #works
    print(colnames(pre_MyData)[i])
    #does not work
    #filtered <- rbind(filtered,colnames(test2_z)[i-1])
  }
  else
  {
    print("no")
  }
  
}

MyData <- (pre_MyData[,-c(filtered)])

fieldOfInterest='yFYield_CSUSHPINSA'

#MyData <- pre_MyData

colnames(MyData) 

nonXFields <- c('test2_z.date','BL_yFYield_CSUSHPINSA','yFYield_CSUSHPINSA')

#xfields (exclude nonXFields)
xList = colnames(MyData[ ,which((names(MyData) %in% nonXFields)==FALSE)])

MR_yField <- 'yFYield_CSUSHPINSA'
BL_yField <- 'BL_yFYield_CSUSHPINSA'

yField = MR_yField

#subset/filter by column
#https://stackoverflow.com/questions/41863722/r-filter-columns-in-a-data-frame-by-a-list
#x <- MyData[ ,which((names(MyData) %in% nonXFields)==FALSE)]

#preserves column names
x <- MyData[xList]
y <- MyData[yField]

nrow(x)
nrow(y)

train_size = .70
#not used
#test_size = 1- train_size

preset_rng <- sample(nrow(MyData), replace=F)
#static training set

#training/validation sets (split into rebootstrapped 1:1 distinct partitions)

#static testing set
#preset2 <- preset_rng[ceiling(nrow(MyData)*train_size):nrow(MyData)]

#dat <- dat[-which(colnames(dat) == 'Met_Color')]
#dat is yxlist
dat <- cbind(y,x)

#setup for classification
BL_yField <- 'BL_yFYield_CSUSHPINSA'
y2 <- MyData[BL_yField]
dat2 <- cbind(y2,x)

#View(colnames(MyData))
vars <- c()
#data is too big for exhaustive search
factor_test_list<-c()
set.seed(256)

print(i)
#lower

#rescramble set1
id.train <- preset_rng[1:floor(nrow(MyData)*train_size)]
id.test = setdiff(1:nrow(dat), id.train) # setdiff gives the set difference

training1Data <- c()
testing1Data <- c()

#new plan is to create two randomized partitions that will have complete dataset algo's done on them.
training1Data <- MyData[id.train, ]  # model training data
testing1Data <- MyData[id.test, ]

train1_xy_set <- c()
test1_xy_set <- c()

train1_xy_set <- training1Data[c(yField,xList)]
test1_xy_set <- testing1Data[c(yField,xList)]

names <- c()

#keep y in front, add x's
names <- c(fieldOfInterest, names)
vars <- xList
names <- c(names, vars)

training1Model <- lm(train1_xy_set)
#testingModel <- lm(test1_xy_set)

#fit <- lm(training1Model, data=test1_xy_set)
#summary(fit)



## below is just an example with manually selected variable: age, fuel type and weight; just for illustration purpose
#obj = lm(Price ~ Age_08_04 + factor(Fuel_Type) + Weight, data = dat[id.train, ])
# dat[id.train, ] takes rows of id.train
# if you put some integer after the comma, it will take the corresponding columns
# for example, if you put dat[, c(1, 5, 3)], it gives you the 1st, 5th, 3rd columns of the data
#plot(obj) # see what happened?!
## Model diagnosis automatically implemented with the basic function plot()
## plot like magic
# residuals vs fitted: check Independence and Homoscedasticity
# Normal Q-Q: check Normality; want to see a straight line
# A good example: http://data.library.virginia.edu/diagnostic-plots/

#summary(obj) # gives the summary of output; you can also just type obj
# check the difference with and without summary()

#names(obj) # check what were saved in obj
#obj$resid
#class(obj$resid) # check the variable type
#plot(obj$resid)
#obj$fitted

# predict the future observation!
#yhat = predict(obj, newdata = dat[id.test, ])
#length(yhat)
#length(id.test) # just to check if the same number of predicted value as id.test is reported

#residual plot line.
plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat, xlab='Actual y', ylab='Fitted y')
abline(0, 1, col='red') # add a line with intercept 0 and slope 1; we want to see points around this line

######################################################
## Additional topic: variable selection (backward, forward, best subset)
# a good resource: http://www.stat.columbia.edu/~martin/W2024/R10.pdf

### forward selection ###
## step 1: fit a null model and a full model first

#can't use dat[[1]] because it will add it to the current dataset, nor fieldOfInterest because it isn't read properly as a string nor toString(fieldOfInterest)
obj.null = lm(yFYield_CSUSHPINSA ~ 1, dat) # only intercept, 1, is included in the model
# obj.full = lm(Price ~ ., dat = dat5[id.train, ]) # if not specifying any independent variables, all other than Price will be independent variables
obj.full = lm(yFYield_CSUSHPINSA ~ .,dat = dat)
## scope is to start from a null model and end with a full model; direction is forward

#these are best final models after stepwise is applied
obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward') # forward selection by Akaike information criterion (AIC)

# Mallows's Cp is equivalent to AIC in the case of (Gaussian) linear regression
### backward elimination ###
obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward') # start with full and end with null; reversed comparing to forward
### stepwise selection ###
obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') # start with full and end with null

expand.grid(colnames(MyData))
summary(obj1) ## ending up with a model with 13 variables
length(obj1$coefficients)
summary(obj2) # end up with a model with 15 variables
length(obj2$coefficients)
summary(obj3) # end up with a model with 12 variables, final model same as backward
length(obj3$coefficients)

data.frame((obj1$coefficients))
data.frame((obj2$coefficients))
data.frame((obj3$coefficients))

#looking at backwards
rownames(data.frame((obj2$coefficients)))

#### Check some other model assumptions
# normality (of the residual)
hist(obj1$resid)
hist(obj2$resid)
hist(obj3$resid)
#View(obj2)
# Homoscedasticity
plot(obj1$resid, obj1$fitted)
plot(obj2$resid, obj2$fitted)
plot(obj2$resid, obj3$fitted)

#use this on final model

# linearity
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3)) # check the difference by removing this line
nc = length(obj2$coefficients)-1
for (i in 2:nc)
{
  plot(dat[[tail(row.names(data.frame(obj2$coefficients)),-1)[i]]], dat[[1]])
}

#correlation
cor(dat[[1]], dat, use='na.or.complete')

layout(matrix(c(1),1,1))
#correlation matrix of backwards
corrplot( cor(dat[c('yFYield_CSUSHPINSA',tail(row.names(data.frame(obj2$coefficients)),-1))], use='na.or.complete'))

#normality
# actually, plot(obj) gives you QQ plot, better way to check it

# plot(obj1) can also give plot like this

# there are some other assumptions such as "No Multicollinearity"
#qq
## A simpler approach to do model diagnosis
layout(matrix(c(1,2,3,4),2,2)) # check the difference by removing this line
plot(obj1)
plot(obj2)
plot(obj3)

yhat1 = predict(obj1, newdata = dat[id.test, ])
#deviations sqrt(deviations squared)
sqrt(mean((dat[id.test, 'yFYield_CSUSHPINSA'] - yhat1)^2, na.rm=T)) ## manually calculate it! same

yhat2 = predict(obj2, newdata = dat[id.test, ])
sqrt(mean((dat[id.test, 'yFYield_CSUSHPINSA'] - yhat2)^2, na.rm=T)) ## manually calculate it! same

yhat3 = predict(obj3, newdata = dat[id.test, ])
sqrt(mean((dat[id.test, 'yFYield_CSUSHPINSA'] - yhat3)^2, na.rm=T)) ## manually calculate it! same

#no need to derive R^2 for every model.  Quick and easy
fwdstep_rmse <- rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat1) ## RMSE for test data
#0.007270917
bckstep_rmse <- rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat2) ## RMSE for test data
#0.006901305
stepboth_rmse <- rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat3) ## RMSE for test data
#0.007343593
fwdstep_rmse
bckstep_rmse
stepboth_rmse

par(mfrow = c(2, 2))

plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat1, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')
plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat2, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')
plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat3, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')

#pick filtered subset from above
# best subset

subsets = regsubsets(yFYield_CSUSHPINSA ~ ., data = dat[id.train, ], nvmax=16)
subsets
## allow up to 20 variables in the model; we should put a constraint like this otherwise it will run forever

# not recommended to use best subset at all

####################################################
# compare prediction results of forward, backward, stepwise

# we choose whichever one of the three methods

bestRMSE = min(fwdstep_rmse,bckstep_rmse, stepboth_rmse)
bestRMSE
#0.009104385
bestModel <- c()

if(fwdstep_rmse == bestRMSE) {bestModel <- obj1}
if(bckstep_rmse == bestRMSE) {bestModel <- obj2}
if(stepboth_rmse == bestRMSE) {bestModel <- obj3}

# model diagnosis #
par(mfrow = c(2, 2))
plot(bestModel) # not bad

bestModel$coefficients

bestModel$terms

yhat = predict(bestModel, newdata=dat[id.test, ])
ytest = dat[id.test, 'yFYield_CSUSHPINSA']
rmse(ytest, yhat)

#test classification using best linear model
yhat.test = rep(0, length(id.test))
yhat.test[yhat > 0] = 1

#https://machinelearningmastery.com/confusion-matrix-machine-learning/

total_predictions = length(id.test)
classification_accuracy = correct_predictions / total_predictions
correct_predictions = sum(yhat.test == dat2$BL_yFYield_CSUSHPINSA[id.test])
error_rate = (1 - (correct_predictions / total_predictions))

#https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
#https://stackoverflow.com/questions/40783331/rocr-error-format-of-predictions-is-invalid
#pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
#pred/prob, actual
pred <- prediction(as.numeric(yhat),as.numeric(dat2$BL_yFYield_CSUSHPINSA[id.test]))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")

#https://machinelearningmastery.com/confusion-matrix-machine-learning/
#results <- confusionMatrix(data=predicted, reference=expected)

#https://stackoverflow.com/questions/30002013/error-in-confusion-matrix-the-data-and-reference-factors-must-have-the-same-nu
cm_bestLinear = confusionMatrix(
  factor(yhat.test, levels = 0:1),
  factor(dat2$BL_yFYield_CSUSHPINSA[id.test], levels = 0:1)
)

#gain chart
#https://stackoverflow.com/questions/38268031/creating-a-lift-chart-in-r
gain <- performance(pred, "tpr", "rpp")

par(mfrow = c(2, 2))
plot(gain, main = "Gain Chart")
plot(roc.perf)
abline(a=0, b= 1)
cm_bestLinear
#lift(yhat.test~dat2$BL_yFYield_CSUSHPINSA[id.test])

## 2. kNN prediction ##

knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=10) {
  vec.rmse = rep(NA, kmax)
  for (k in 1:kmax) {
    yhat.test = knn.reg(Xtrain, Xtest, ytrain, k)$pred
    vec.rmse[k] = rmse(yhat.test, ytest)
  }
  list(k.opt = which.min(vec.rmse), rmse.min = min(vec.rmse), vec.rmse)
}

#knn.reg.bestK(x_training, x_test, y_training, y_test)
knn_model <- knn.reg.bestK(dat[id.train, ], dat[id.test, ], dat$yFYield_CSUSHPINSA[id.train], dat$yFYield_CSUSHPINSA[id.test])

knn_model$k.opt
#predictions
knn.reg(dat[id.train, ], test = dat[id.test, ], dat$yFYield_CSUSHPINSA[id.train], k = knn_model$k.opt)
#knn_model

#kNNdistplot(dat[id.train, ], k=knn_model$k.opt)

#cl <- dbscan(dat[id.train, ], eps = .5, minPts = knn_model$k.opt)
#pairs(dat[id.train, ], col = cl$cluster+1L)

##0.01694881

#test classification using knn
yhat = knn.reg(dat[id.train, ], dat[id.test, ],dat$yFYield_CSUSHPINSA[id.train], knn_model$k.opt)
yhat.test = rep(0, length(id.test))
yhat.test[yhat$pred > 0] = 1
mean(yhat.test != dat2$BL_yFYield_CSUSHPINSA[id.test])

## 3. Regression Tree ##
fit = rpart(yFYield_CSUSHPINSA~., method="anova", data=dat[id.train,])
par(mfrow=c(1,1))
rpart.plot(fit, roundint=FALSE)

#drop 1st column from prediction x's
yhat.test = predict(fit, newdata = dat[id.test,-1])
rmse(yhat.test, ytest)
#0.02422354

#test classification using regression tree
class.test = rep(0, length(id.test))
class.test[yhat.test > 0] = 1
mean(class.test != dat2$BL_yFYield_CSUSHPINSA[id.test])
#0.2777778

#### looks like MLR is the best one! ####
round(summary(bestModel)$coef, 3)

## 1. Logistic Regression ##
obj_LR.null = glm(BL_yFYield_CSUSHPINSA ~ 1, data = dat2, family = 'binomial')
obj_LR.full = glm(BL_yFYield_CSUSHPINSA ~ ., data = dat2, family = 'binomial')
full.formula = formula(obj_LR.full)

obj4 = step(obj.null, direction='forward', scope=full.formula) # it will print out models in each step
summary(obj4) # it will give you the final model

#output predictions
#drop 1st column from prediction x's
prob.hat = predict(obj4, newdata=dat2[id.test, -1], type='response')
hist(prob.hat)

#create array and initialize full size [of id.test] to 0
yhat.test = rep(0, length(id.test))

#0 error
#how many don't equal correctly guessed
mean(yhat.test != dat2$BL_yFYield_CSUSHPINSA[id.test])
# 0.66

#lift.chart(modelList, data, targLevel, trueResp, type = "cumulative", sub = "")

## 2. kNN classification ##
knn.bestK = function(train, test, y.train, y.test, k.max = 20) {
  k.grid = seq(1, k.max, 2)
  fun.tmp = function(x) {
    y.hat = knn(train, test, y.train, k = x, prob=F)
    return(sum(y.hat != y.test))
  }
  error = unlist(lapply(k.grid, fun.tmp))/length(y.test)
  out = list(k.optimal = k.grid[which.min(error)], error.min = min(error), error)
  return(out)
}


knn_bestK_model <- knn.bestK(dat[id.train, ], dat[id.test, ], dat2$BL_yFYield_CSUSHPINSA[id.train], dat2$BL_yFYield_CSUSHPINSA[id.test])
knn_bestK_model


#plotcluster(dat, clus$cluster)

#k=5
##0.33

#predictions
yhat = knn(dat[id.train, ], dat[id.test, ], knn_bestK_model$k.optimal)
#test classification using knn
yhat.test = rep(0, length(id.test))
yhat.test[yhat$pred > 0] = 1
mean(yhat.test != dat2$BL_yFYield_CSUSHPINSA[id.test])

## 3. Classification Tree ##
fit = rpart(BL_yFYield_CSUSHPINSA~., method="class", data=dat2[id.train,])
par(mfrow=c(1,1))
rpart.plot(fit,roundint=FALSE)

#2 is to remove 2nd column, end point is to have only one column, 
prob.pred = predict(fit, newdata = dat2[id.test,-1])[,2]
yhat.test = rep(0, length(id.test))
yhat.test[prob.pred > .5] = 1
mean(yhat.test != dat2$BL_yFYield_CSUSHPINSA[id.test])
#0.33

#### looks like logistic regression is the best one! ####
round(summary(obj4)$coef, 3)
out = data.frame(summary(obj4)$coef)
out$OR = exp(out[,1])
round(out, 3)
