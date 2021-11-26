#packages
{
  packages_github <- c()
  
  installed_packages <- packages_github %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    devtools::install_github(packages_github[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(sub(".*?/", "", packages_github), library, character.only = TRUE))
  
  packages <- c("dplyr","ggplot2","quantmod","zoo","plyr","forecast","tseries","tidyverse","furrr")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

plan(multisession, workers = 4)

#functions
{
  shift <- function(df, sv = 1) df[c((nrow(df)-sv+1):nrow(df), 1:(nrow(df)-sv)),]
}

lacondos <- read.csv("C:/Users/User/Documents/wiki/wiki/Excel/LACondos.csv",row.names=1)

dates <- as_data_frame(as.Date(rownames(lacondos), format =  "%m/%d/%Y"))
f = periodicity(dates$value)

fresult = switch(  
  f$scale,  
  "monthly"= 12,  
  "daily"= 252,  
  "quarterly"= 4,  
  "weekly"= 52,
)  

{
  training <- nrow(lacondos)*.7
  validation <- training-round(nrow(lacondos)*.7*.3,0)
  holdout <- nrow(lacondos)-nrow(lacondos)*.7+validation
}

lacondos$MA_trend <- lag(zoo::rollmean(lacondos$LXXRCSA, k = fresult, fill = NA, align="center"), n=1)
lacondos$CMA <- zoo::rollmean(lacondos$MA_trend, k = fresult/2, fill = NA, align="left")
lacondos$time <- 1:nrow(lacondos)

CMA_df <- lacondos[,c("time","CMA")][complete.cases(lacondos[,c("time","CMA")]),]

CMA_df_lm <- lm(CMA ~ time, data=CMA_df)

lacondos$CMAT <- 1:nrow(lacondos)*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1]
  
lacondos$CF <- lacondos$CMA/lacondos$CMAT

#View(lacondos)
lacondos$Date <- dates$value
lacondos$Month <- months(dates$value)

lacondos$Seasonal_Ratios <- lacondos$LXXRCSA/lacondos$CMA
Seasonal_Indexes <- aggregate(Seasonal_Ratios ~ Month, lacondos , mean ) 
Seasonal_Indexes_Adj <- as_data_frame(Seasonal_Indexes$Seasonal_Ratios/mean(Seasonal_Indexes$Seasonal_Ratios))
Seasonal_Indexes_Adj$Month = Seasonal_Indexes$Month

lacondos <- lacondos %>% left_join(Seasonal_Indexes_Adj, by = c('Month' = 'Month'))

lacondos <- rename(lacondos,c("value" = "SI_Mult"))

a <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="additive")

m <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="multiplicative")

plot(a)
plot(a$trend)

N=length(lacondos$CF)
alpha = .05
-qnorm((1-alpha)/2)

ciz = c(-1,1)+(-qnorm((1-alpha)/2)/sqrt(N-3))
cir = (exp(2*ciz)-1)/(exp(2*ciz)+1)  
                      
significant_threshold=(exp(2*1.96/sqrt(N-3)-1))/(exp(2*1.96/sqrt(N-3)+1))
                       
ac_f <- acf(na.omit(lacondos$CF))

if(any(ac_f$acf>significant_threshold))
{
  print("trend")
}
lines(lacondos$CF)

tail(lacondos,12)

ar <- auto.arima(na.omit(ts((lacondos$CF))),trace=FALSE,parallel = TRUE,stepwise=FALSE)

plot(ar$residuals)
acf(ar$residuals)
pacf(ar$residuals)



temp <- as.data.frame(as.matrix(forecast(ar)$mean))

forecastWindow <- nrow(temp)

rownames(temp) <- tail(rownames(lacondos[!complete.cases(lacondos$CF),]),forecastWindow)
colnames(temp) <- 'CF'

lacondos$CF[match(rownames(temp),rownames(lacondos))] = temp$CF

sum(na.omit(ar$residuals)^2)

lacondos$forecast_M <- lacondos$CMAT*lacondos$CF*lacondos$`SI_Mult`
lacondos$forecast_A <- lacondos$CMAT+lacondos$CF+lacondos$`SI_Mult`

plot(lacondos$forecast)

View(lacondos)