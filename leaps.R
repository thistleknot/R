library(whitening)
library(DMwR)
library(bnstruct)
library(relaimpo)
library(car)
library(psych)
library(tidyr)
library(ggplot2)
library(leaps)

#I have CV leaps
#the reason I'm comparing this is due to the leaps algorithm that is implemented that is supposedly faster than bestglm

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}


data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]

data3 <- data.frame(scale(data2))

rep=100
rmse.cvr <- c()

#as.formula(object$call[[2]]) doesn't lend itself to mclapply very well
for(r in 1:rep)
{
  nfolds=10
  
  folds=sample(rep(1:nfolds, length=nrow(data3)))
  
  cv.errors=matrix(NA, nfolds, (ncol(data3)-1))
  for (k in 1:nfolds){
    best.fit=regsubsets(Poverty~., data=data3[folds!=k,], nvmax=(ncol(data3)-1), method="exhaustive")
    for (i in 1:(ncol(data3)-1)){
      #print(i)
      pred=predict.regsubsets(best.fit, data3[folds==k,],id=i)
      cv.errors[k,i]=mean((data3$Poverty[folds==k]-pred)^2)
    }
  }
  
  rmse.cv=sqrt(apply(cv.errors, 2, mean))
  rmse.cvr <- rbind(rmse.cv,rmse.cvr)
}

scores <- unlist(lapply(1:ncol(rmse.cvr),function(x)
  {
  mean(rmse.cvr[,x])
}))

plot(scores, pch=19, type="b")

d <- leaps(x=as.matrix(data3[,-1]), y=as.matrix(data3[,1]), int=TRUE, method=c("Cp", "adjr2", "r2"), nbest=1,names=NULL, df=NROW(x), strictly.compatible=TRUE)

#summary(d)$which[id,-1]
selectedPredictors <- colnames(data3[,-1][,d$which[which.min(scores),]])

wd <- whiten(as.matrix(data3[,-1][selectedPredictors]), method=c("ZCA"))
colnames(wd) <- selectedPredictors

whiteData <- cbind(data3[,1,drop=FALSE],wd)

fit2 <- lm(whiteData)

summary(fit2)
         
calc.relimp(fit2,type=c("lmg","last","first","pratt"),
           rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit2, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                   diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

sortedNames <- names(sort(abs(fit2$coefficients)[-1],decreasing=TRUE))

whiteData <- cbind(whiteData[,1,drop=FALSE],whiteData[sortedNames])

barplot(sort((round(abs(fit2$coefficients)[-1],2)/sum(round(abs(fit2$coefficients)[-1],2))),decreasing=TRUE)) 

hist(fit2$residuals)
plot(fit2,2)

pairs.panels(whiteData,method = "pearson", # correlation method
            pch=21)


whiteData %>% gather() %>% head()

ggplot(gather(whiteData), aes(value)) + 
 geom_histogram(bins = 10) + 
 facet_wrap(~key, scales = 'free_x')
