library(forecast)

var_of_int <- "MSPUS"

combo <- read.csv("C:/Users/User/Documents/wiki/wiki/dev/python/Python-Stock/data/deltas.csv",row.names=1,header=TRUE)

critical.r <- function( n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(critical.r)
}
# Example usage: Critical correlation coefficient at sample size of n = 100

names <- c()
lags <- c()

{
  training <- combo[1:floor(nrow(combo)*.7),]
  #validation <- training-round(nrow(combo)*.7*.3,0)
  holdout <- combo[floor(nrow(combo)-nrow(combo)*.7+validation):nrow(combo),]
}

for(c in 1:(length(colnames(training))))
{#c=3

      ccf1 <- ccf(training[,var_of_int,drop=FALSE],training[,c], lag = 4, correlation=TRUE, p1=TRUE)
  
      upperCI <- qnorm((1+0.95)/2)/sqrt(ccf1$n.used)
      lowerCI <- -qnorm((1+0.95)/2)/sqrt(ccf1$n.used)
      
      ind.max <- which(abs(ccf1$acf)==max(abs(ccf1$acf)))
      max.cor <- ccf1$acf[ind.max]
      lag.opt <- ccf1$lag[ind.max] 
      
      if(max.cor>critical.r( ccf1$n.used ))
      {
        
        if(lag.opt>0)
        {
          names <- c(names,(colnames(training)[c]))
          print(colnames(training)[c])
          print(max.cor)
          print(lag.opt)
          lags <- c(lags,lag.opt)
          print(2 * (1 - pnorm(abs(max.cor), mean = 0, sd = 1/sqrt(ccf1$n.used))))
        }
        
      }
}

#lag example
cor(na.omit(cbind(training[,var_of_int,],dplyr::lag(training[,"IBM"],1))))

newDF <- combo[,var_of_int,drop=FALSE]

for (n in 1:length(names))
{
  newDF <- cbind(newDF,dplyr::lag(combo[,names[n],drop=FALSE],n=abs(lags[n])))
  
}


newDF_t <- newDF[1:nrow(training),]
newDF_h <- newDF[(nrow(training)+1):nrow(combo),]

cor(na.omit(newDF[,1:2,]))

#View(newDF)

horizon = 5

lm_ <- lm(MSPUS ~., data=na.omit(newDF_t))

summary(lm_)

#plot(lm_)

checkresiduals(lm_$residuals)

test_1 <- auto.arima(lm_$residuals)

f_1 <- as.data.frame(forecast(test_1,h=horizon)+lm_$fitted.values)

rownames(f_1) <- as.integer(rownames(f_1))+abs(as.integer(rownames(f_1))-nrow(training))[1]+1

error_1 <- mean(abs(f_1[,1]- newDF_h[,var_of_int][1:nrow(f_1)]))

test_2 <- auto.arima(lm_$model[,var_of_int,drop=FALSE], xreg = as.matrix(na.omit(newDF_t[,colnames(lm_$model)[1:length(colnames(lm_$model))]])))

f_2 <- as.data.frame(forecast(test_2, h=horizon, xreg = as.matrix(newDF_h[1:horizon,colnames(lm_$model)[1:length(colnames(lm_$model))]])))

rownames(f_2) <- as.integer(rownames(f_2))+abs(as.integer(rownames(f_2))-nrow(training))[1]+1

error_2 <- mean(abs(f_2[,1] - newDF_h[,var_of_int][1:nrow(f_2)]))

print(error_1)

print(error_2)

#plot(f_1[,1],holdout[,var_of_int][1:nrow(f_2)])

#plot(f_2[,1],holdout[,var_of_int][1:nrow(f_2)])


f_1

f_2


newDF_h[1:horizon,1,drop=FALSE]