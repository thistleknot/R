
packages <- c("dplyr","ggplot2","quantmod","zoo","plyr","forecast","tseries","tidyverse","furrr","lubridate","anytime","caret","ppcor")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

raw <- read.csv("C:/Users/User/Documents/wiki/wiki/dev/python/Python-Stock/data/combined_set.csv",row.names=1,header=TRUE)

f = periodicity(as.Date(anytime(rownames(raw))))

critical.r <- function( n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(critical.r)
}

`%notin%` <- Negate(`%in%`)

seasonal = switch(  
  f$scale,  
  "monthly"= 12,  
  "daily"= 252,  
  "quarterly"= 4,  
  "weekly"= 52,
)  

var_of_int <- "MSPUS"

#starts at 2:
sndif_ <- c()

for (n in 1:(length(colnames(raw))))
{
  #print(n)
  d_ <- nsdiffs(ts(raw[,n],frequency=seasonal))
  
  #print(d_)
  sndif_ <- c(sndif_,d_)
}

combo_s <- data.frame(matrix(ncol = 1, nrow = nrow(raw)))

for (d in 1:length(sndif_))
{#d=1
  #print(d)
  if(sndif_[d]*seasonal == 0)
  {
    combo_s <- cbind(combo_s,raw[,d,drop=FALSE])
  }else
  {
    combo_s <- cbind(combo_s,raw[,d,drop=FALSE]-dplyr::lag(raw[,d,drop=FALSE],sndif_[d]*seasonal))
  }
  
}

combo_s = subset(combo_s, select = -c(1) )

rownames(combo_s) <- rownames(raw)

#ncol(combo_s)
#ncol(raw)

#View(combo_s)

ndif_ <- c()

for (n in 1:(length(colnames(combo_s))))
{
  #print(n)
  d_ <- ndiffs(combo_s[,n])
  
  ndif_ <- c(ndif_,d_)
}

combo_d <- data.frame(matrix(ncol = 1, nrow = nrow(raw)))

for (d in 1:length(ndif_))
{
  #print(d)
  if(ndif_[d] == 0)
  {
    #print("TRUE")
    combo_d <- cbind(combo_d,combo_s[,d,drop=FALSE])
  }else
  {
    combo_d <- cbind(combo_d,(combo_s[,d,drop=FALSE]-dplyr::lag(combo_s[,d,drop=FALSE],1)))
  }
  
}

combo_d = subset(combo_d, select = -c(1) )

rownames(combo_d) <- rownames(raw)

#ncol(combo_d)

#View(combo_d)

# Example usage: Critical correlation coefficient at sample size of n = 100

names <- c()
lags <- c()

combo_ <- na.omit(combo_d)

{
  training <- combo_[1:floor(nrow(combo_)*.7),]
  #validation <- training-round(nrow(combo_)*.7*.3,0)
  holdout <- combo_[floor(nrow(combo_)-nrow(combo_)*.7+validation):nrow(combo_),]
}

for(c in 1:(length(colnames(training))))
{#c=3
  
  ccf1 <- ccf(training[,var_of_int,drop=FALSE],training[,c], lag = seasonal, correlation=TRUE, p1=TRUE)
  
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

newDF <- combo_[,var_of_int,drop=FALSE]

for (n in 1:length(names))
{
  newDF <- cbind(newDF,dplyr::lag(combo_[,names[n],drop=FALSE],n=abs(lags[n])))
  
}

newDF_t <- na.omit(newDF[1:nrow(training),])
newDF_h <- na.omit(newDF[(nrow(training)+1):nrow(combo_),])

cor(na.omit(newDF[,1:2,]))

sig_table = matrix(0, ncol=ncol(newDF_t))
colnames(sig_table) <- colnames(newDF_t)
signs_table = matrix(0, ncol=ncol(newDF_t))
colnames(signs_table) <- colnames(newDF_t)

p_threshold = .05

New_Names = colnames(newDF_t)[2:length(colnames(newDF_t))]
iteration=0

dat <- 1:10
n=length(dat)

lapply(folds,length)

folds<-createTimeSlices(y=rownames(newDF_t),initialWindow = 20,horizon = 10)

exclude <- c()

#crit <- critical.r(nrow(set_), .05)

for (k in 1:length(folds$train))
{#k=1
  max_pvalue = 1
  
  subset = newDF_t[(folds$train[k][[1]]),c(colnames(newDF_t) %notin% c(exclude))]
  
  set_ = subset[,c(colnames(newDF_t) %notin% c(var_of_int))]
  
  while(max_pvalue>=.05)
  {
    p_values  <- pcor(subset)$p.value[,var_of_int,drop=FALSE]
    
    max_pname = rownames(p_values)[which.max(p_values)]
    max_pvalue = p_values[max_pname,]
    
    if (max_pvalue >= .05)
    {
      print(max_pname)
      subset <- dplyr::select(subset,-c(max_pname))
      
    }
  }
    
  winners = rownames(p_values)[rownames(p_values) %notin% c(var_of_int)]
  sig_table = sig_table + as.integer(colnames(newDF_t) %in% winners)
  
  t_ <- t(pcor(subset[,c(var_of_int,winners)])$estimate[,var_of_int,drop=FALSE])[,-1]
  rownames(t_) <- rownames(signs_table)

  temp_ <- merge(t(signs_table), t_, by=0,all.x=TRUE)
  rownames(temp_) <- temp_$Row.names
  signs_table_ = rowSums(temp_[,2:3],na.rm=TRUE)
  signs_table_ = ifelse(signs_table_==0,0,ifelse(signs_table_<0,-1,1))
  signs_table = signs_table_ + signs_table
  
}

keepers = colnames(sig_table)[sig_table>=(length(folds$train)/2)]

horizon = 5

f <- as.formula(paste(var_of_int, " ~."))

lm_ <- lm(f, data=na.omit(newDF_t[,c(var_of_int,keepers)]))

summary(lm_)

checkresiduals(lm_$residuals)

actual <- newDF_h[1:horizon,var_of_int,drop=FALSE]

test_0 <- auto.arima(newDF_t[,var_of_int,drop=FALSE])

f_0 <- as.data.frame(forecast(test_0,h=horizon))

test_1 <- auto.arima(lm_$residuals)

f_1 <- as.data.frame(forecast(test_1,h=horizon))

f_1a <- f_1 + t(predict(lm_,newdata=newDF_h[1:horizon,keepers,drop=FALSE]))

rownames(f_1a) <- rownames(actual)

test_2 <- arfima(lm_$residuals)

f_2 <- as.data.frame(forecast(test_2,h=horizon))

f_2a <- f_2 + t(predict(lm_,newdata=newDF_h[1:horizon,keepers,drop=FALSE]))

rownames(f_1a) <- rownames(actual)
rownames(f_2a) <- rownames(actual)

e_f_2a <- mean(abs(f_2a$`Point Forecast`-t(actual)))
e_f_1a <- mean(abs(f_1a$`Point Forecast`-t(actual)))

e_f_0 <- mean(abs(f_0$`Point Forecast`-t(actual)))

print(paste("error forecast 1:", e_f_2a))

print(paste("error forecast 1:", e_f_1a))

print(paste("error forecast 0:", e_f_0))

