library(whitening)
library(DMwR)
library(bnstruct)
library(bestglm)
library(relaimpo)
library(car)
library(psych)
library(tidyr)
library(ggplot2)

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]

data3 <- data.frame(scale(data2))

data3 

ptm <- proc.time()
chosen <- bestglm(cbind(data3[,-1],data3[,1,drop=FALSE]), IC = "CV")
timed <- proc.time() - ptm
print(timed)

selectedPredictors <- names(chosen$BestModel$coefficients)[-1]

fit <- lm(cbind(data3[,1,drop=FALSE],data3[selectedPredictors]))

#plot(fit)

wd <- whiten(as.matrix(data3[,-1][selectedPredictors]), method=c("ZCA"))
colnames(wd) <- selectedPredictors

whiteData <- cbind(data3[,1,drop=FALSE],wd)

fit2 <- lm(whiteData)

#plot(fit2)

summary(fit)

summary(fit2)

calc.relimp(fit2,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit2, b = 1000, type = c("lmg",
                                             "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

plot(fit,2)
plot(fit2,2)
plot(fit2$residuals-fit$residuals)

sortedNames <- names(sort(abs(fit2$coefficients)[-1],decreasing=TRUE))

barplot(sort((round(abs(fit2$coefficients)[-1],2)/sum(round(abs(fit2$coefficients)[-1],2))),decreasing=TRUE)) 

whiteData <- cbind(whiteData[,1,drop=FALSE],whiteData[sortedNames])

hist(fit2$residuals)

pairs.panels(whiteData,method = "pearson", # correlation method
             pch=21)


whiteData %>% gather() %>% head()

ggplot(gather(whiteData), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
