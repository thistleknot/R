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

aggresult = switch(  
  f$scale,  
  "monthly"= "Month",  
  "daily"= "Day",  
  "quarterly"= "Quarter",  
  "weekly"= "Week",
)

aset = switch(  
  f$scale,  
  "monthly"= c("Month" = "Month"),  
  "daily"= c("Day" = "Day"),  
  "quarterly"= c("Quarter" = "Quarter"),  
  "weekly"= c("Week" = "Week"),
) 

{
  training <- nrow(lacondos)*.7
  validation <- training-round(nrow(lacondos)*.7*.3,0)
  holdout <- nrow(lacondos)-nrow(lacondos)*.7+validation
}

#vars
{
  variable_of_interest="LXXRCSA"
  arima_choices = c("arfima","arima")
  arima_choice = arima_choices[1]
  N=nrow(lacondos)
  alpha = .05
  significant_threshold=(exp(2*-qnorm((1-alpha)/2)/sqrt(N-3)-1))/(exp(2*-qnorm((1-alpha)/2)/sqrt(N-3)+1))
}

lacondos$better_CMA <- ma(lacondos[,variable_of_interest,drop=FALSE], order = fresult, centre=T)

lacondos$MA_trend_cycle <- lag(zoo::rollmean(lacondos[,variable_of_interest,drop=FALSE], k = fresult, fill = NA, align="center"), n=1)
lacondos$CMA <- zoo::rollmean(lacondos$MA_trend_cycle, k = fresult/2, fill = NA, align="left")
lacondos$time <- 1:nrow(lacondos)

class(lacondos$better_CMA) <- class(lacondos$CMA)

CMA_df <- lacondos[,c("time","CMA")][complete.cases(lacondos[,c("time","CMA")]),]

CMA_df_lm <- lm(CMA ~ time, data=CMA_df)

lacondos$CMAT <- 1:nrow(lacondos)*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1]
  
lacondos$linear_M_CF <- lacondos$CMA/lacondos$CMAT

lacondos$linear_A_CF <- lacondos$CMA-lacondos$CMAT

lacondos$Date <- dates$value

lacondos[,aggresult] = switch(  
  f$scale,  
  "monthly"= months(dates$value),  
  #"daily"= lacondos[,aggresult] <- weekdays(dates$value)
  "quarterly"= quarters(dates$value),  
  #"weekly"= c("Week" = "Week"),
) 

Linear_A_Seasonal_Index <- lacondos[,variable_of_interest] - lacondos$better_CMA
colnames(Linear_A_Seasonal_Index) <- "Linear_A_Seasonal_Index"
Linear_A_Seasonal_Index <- cbind(Linear_A_Seasonal_Index,lacondos[,aggresult,drop=FALSE])
#colnames(Linear_A_Seasonal_Indexes) <- c("Linear_A_Seasonal_Indexes",aggresult)

f1 <- as.formula(paste("Linear_A_Seasonal_Index", " ~ ", aggresult))

Linear_A_Seasonal_Indexes <- aggregate(f1, na.omit(Linear_A_Seasonal_Index) , mean)

Linear_A_Seasonal_Indexes_Adj <- Linear_A_Seasonal_Indexes[,"Linear_A_Seasonal_Index",drop=FALSE]-mean(Linear_A_Seasonal_Indexes[,"Linear_A_Seasonal_Index"])
colnames(Linear_A_Seasonal_Indexes_Adj) <- "Linear_A_Seasonal_Indexes_Adj"
rownames(Linear_A_Seasonal_Indexes_Adj) <- Linear_A_Seasonal_Indexes[,aggresult]

Linear_A_Seasonal_Indexes$Linear_A_Seasonal_Indexes_Adj = Linear_A_Seasonal_Indexes_Adj

Linear_A_Seasonal_Indexes_Adj[,aggresult] = Linear_A_Seasonal_Indexes[,aggresult]

lacondos <- lacondos %>% left_join(Linear_A_Seasonal_Indexes_Adj, by = aset)

Linear_M_Seasonal_Ratios <- lacondos[,variable_of_interest]/lacondos$better_CMA
Linear_M_Seasonal_Ratios <- cbind(Linear_M_Seasonal_Ratios,lacondos[,aggresult,drop=FALSE])

f2 <- as.formula(paste("Linear_M_Seasonal_Ratios", " ~ ", aggresult))
Linear_M_Seasonal_Indexes <- aggregate(f2, Linear_M_Seasonal_Ratios, mean)

Linear_M_Seasonal_Indexes_Adj <- as.data.frame(Linear_M_Seasonal_Indexes[,"Linear_M_Seasonal_Ratios"]/mean(Linear_M_Seasonal_Indexes[,"Linear_M_Seasonal_Ratios"]))
colnames(Linear_M_Seasonal_Indexes_Adj) <- "Linear_M_Seasonal_Indexes_Adj"
Linear_M_Seasonal_Indexes$Linear_M_Seasonal_Indexes_Adj = Linear_M_Seasonal_Indexes_Adj

Linear_M_Seasonal_Indexes_Adj[,aggresult] = Linear_M_Seasonal_Indexes[,aggresult]

lacondos <- lacondos %>% left_join(Linear_M_Seasonal_Indexes_Adj, by = aset)
colnames(lacondos)

plot(lacondos$linear_M_CF)

tail(lacondos,12)

ac_f <- acf(na.omit(lacondos[,1,drop=FALSE]))

if(any(ac_f$acf>significant_threshold))
{
  print("model linear trend")
}

a_model_nonlinear <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="additive")
m_model_nonlinear <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="multiplicative")

if (arima_choice == "arima")
{
  ar_decompose_a_nonlinear <- auto.arima((na.omit(a_model_nonlinear$trend)))
  ar_decompose_m_nonlinear <- auto.arima((na.omit(m_model_nonlinear$trend)))
}else
  
{
  ar_decompose_a_nonlinear <- arfima((na.omit(a_model_nonlinear$trend)))
  ar_decompose_m_nonlinear <- arfima((na.omit(m_model_nonlinear$trend)))
}

length(na.omit(a_model_nonlinear$trend))
length(a_model_nonlinear$trend)

sum(a_model_nonlinear$residuals^2)
sum(m_model_nonlinear$residuals^2)

if (arima_choice == "arima")
{
  ar_CF_M_linear <- auto.arima((((na.omit(lacondos$linear_M_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
  ar_CF_A_linear <- auto.arima((((na.omit(lacondos$linear_A_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
}else
  
{
  ar_CF_M_linear <- arfima((((na.omit(lacondos$linear_M_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
  ar_CF_A_linear <- arfima((((na.omit(lacondos$linear_A_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
}

length((na.omit(lacondos$linear_M_CF)))
length(((lacondos$linear_M_CF)))
       
plot(ar_CF_M_linear$residuals)
acf(ar_CF_M_linear$residuals)
pacf(ar_CF_M_linear$residuals)

plot(ar_CF_A_linear$residuals)
acf(ar_CF_A_linear$residuals)
pacf(ar_CF_A_linear$residuals)

ar_CF_M_linear_forecast <- (as.matrix(forecast(ar_CF_M_linear)$mean))
ar_CF_A_linear_forecast <- (as.matrix(forecast(ar_CF_A_linear)$mean))

ar_trendcycle_a_nonlinear_forecast <- as.data.frame(as.matrix(forecast(ar_decompose_a_nonlinear)$mean))
ar_trendcycle_m_nonlinear_forecast <- as.data.frame(as.matrix(forecast(ar_decompose_m_nonlinear)$mean))

nonlinearadditional = length(na.omit(a_model_nonlinear$trend))-length((na.omit(lacondos$linear_M_CF)))

forecastWindow <- nrow(ar_CF_M_linear_forecast)
nonlinearforecastWindow = forecastWindow + nonlinearadditional

rownames(ar_CF_M_linear_forecast) <- (tail(as.integer(rownames(na.omit(as.data.frame(lacondos$linear_M_CF)))),1)+1):((tail(as.integer(rownames(na.omit(as.data.frame(lacondos$linear_M_CF)))),1)+nrow(ar_CF_M_linear_forecast)))
colnames(ar_CF_M_linear_forecast) <- 'CF_linear_M_forecast'

rownames(ar_CF_A_linear_forecast) <- (tail(as.integer(rownames(na.omit(as.data.frame(lacondos$linear_M_CF)))),1)+1):((tail(as.integer(rownames(na.omit(as.data.frame(lacondos$linear_M_CF)))),1)+nrow(ar_CF_M_linear_forecast)))
colnames(ar_CF_A_linear_forecast) <- 'CF_linear_A_forecast'

lacondos$linear_M_CF[match(rownames(ar_CF_M_linear_forecast),rownames(lacondos))] =ar_CF_M_linear_forecast
lacondos$linear_A_CF[match(rownames(ar_CF_A_linear_forecast),rownames(lacondos))] =ar_CF_A_linear_forecast

sum(na.omit(ar_CF_M_linear$residuals)^2)
sum(na.omit(ar_CF_A_linear$residuals)^2)

lacondos$forecast_ar_M_linear <- lacondos$CMAT*lacondos$linear_M_CF*lacondos$`Linear_M_Seasonal_Indexes_Adj`
lacondos$forecast_ar_A_linear <- lacondos$CMAT+lacondos$linear_A_CF+lacondos$`Linear_A_Seasonal_Indexes_Adj`

lacondos$forecast_ar_A_nonlinear <- ts(a_model_nonlinear$trend)+ts(a_model_nonlinear$seasonal)
lacondos$forecast_ar_M_nonlinear <- ts(m_model_nonlinear$trend)*ts(m_model_nonlinear$seasonal)

plot(lacondos$forecast_ar_M_linear,type="l",col="purple")
lines(lacondos$forecast_ar_A_linear,col="red")
lines(lacondos[,variable_of_interest],col="black")

View(lacondos)