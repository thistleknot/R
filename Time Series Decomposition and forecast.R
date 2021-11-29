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
  
  forecastWindow <- fresult*3
  
  variable_of_interest="LXXRCSA"
  arima_choices = c("arfima","arima")
  arima_choice = arima_choices[2]
  
  trend_models = c("linear", "poly", "exp")
  trend_model = trend_models[1]
  
  #polynomial order
  porder = fresult*3
  
  #arima is meant for linear trends?
  if(FALSE)
  {
    if(trend_model!="linear")
    {
      arima_choice = arima_choices[2]
    }
  }

  N=nrow(lacondos)
  alpha = .05
  significant_threshold=(exp(2*-qnorm((1-alpha)/2)/sqrt(N-3)-1))/(exp(2*-qnorm((1-alpha)/2)/sqrt(N-3)+1))
}

#true_CMA = centered average halving tail months (13 months)
#CMA is Dr. Zerom's centered average using only 12 months, then 6 months
lacondos$true_CMA <- ma(lacondos[,variable_of_interest,drop=FALSE], order = fresult, centre=T)

lacondos$MA_trend_cycle <- lag(zoo::rollmean(lacondos[,variable_of_interest,drop=FALSE], k = fresult, fill = NA, align="center"), n=1)
lacondos$CMA <- zoo::rollmean(lacondos$MA_trend_cycle, k = fresult/2, fill = NA, align="left")
lacondos$time <- 1:nrow(lacondos)

class(lacondos$true_CMA) <- class(lacondos$CMA)

CMA_df <- lacondos[,c("time","true_CMA")][complete.cases(lacondos[,c("time","true_CMA")]),]

lacondos$date <- rownames(lacondos)

#lacondos$Date <- dates$value

#CMAT
if (trend_model=="linear")
{
  
  CMA_df_lm <- lm(true_CMA ~ time, data=CMA_df) 
  
  temp_df <- data.frame(1:(nrow(lacondos)+forecastWindow)*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1])
  
  colnames(temp_df) <- "CMAT"
  
  temp_df$time <- rownames(temp_df)
  
  temp_df_ <- merge(lacondos, temp_df, by="time", all=TRUE) 
  
  lacondos <- temp_df_[order(as.numeric(temp_df_$time)),]
  
  lacondos$linear_M_CF <- lacondos$true_CMA/lacondos$CMAT
  
  lacondos$linear_A_CF <- lacondos$true_CMA-lacondos$CMAT
  
} else
if (trend_model == "poly") {
  y <- CMA_df[,"true_CMA",drop=TRUE]
  colnames(y) <- "true_CMA"
  
  q <- poly(CMA_df[,"time",drop=TRUE],porder)
  
  newq <- poly(1:(nrow(lacondos)+forecastWindow),porder)
  
  CMA_df_lm <- lm(true_CMA~.,data=as.data.frame(cbind(CMA_df[,"true_CMA",drop=FALSE],q)))
  
  temp_df <- as.data.frame(predict(CMA_df_lm,newdata=newq,type="response"))
  
  colnames(temp_df) <- "CMAT"
  
  temp_df$time <- rownames(temp_df)
  
  temp_df_ <- merge(lacondos, temp_df, by="time", all=TRUE) 
  
  lacondos <- temp_df_[order(as.numeric(temp_df_$time)),]
  
  lacondos$linear_M_CF <- lacondos$true_CMA/lacondos$CMAT
  
  lacondos$linear_A_CF <- lacondos$true_CMA-lacondos$CMAT
  
} else
{
  CMA_df_lm <- lm(log(true_CMA) ~ time, data=CMA_df) 
  
  temp_df <- as.data.frame(exp(1:(nrow(lacondos)+forecastWindow)*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1]))
  
  colnames(temp_df) <- "CMAT"
  
  temp_df$time <- rownames(temp_df)
  
  temp_df_ <- merge(lacondos, temp_df, by="time", all=TRUE) 
  
  lacondos <- temp_df_[order(as.numeric(temp_df_$time)),]
  
  lacondos$linear_M_CF <- lacondos$true_CMA/lacondos$CMAT
  
  lacondos$linear_A_CF <- lacondos$true_CMA-lacondos$CMAT
  
}

rownames(lacondos) <- lacondos$time

lacondos <- select(lacondos, ,-c("time"))

#inputs preceding 0 before months...
for (d in ((nrow(lacondos)+1)-(fresult*3)):nrow(lacondos))
{#d=nrow(lacondos)
  delta_date <- ((nrow(lacondos))-(fresult*3))
  
  temp_date <- as.Date(lacondos[(nrow(lacondos)-(fresult*3)),]$date,format = "%m/%d/%Y")+months(d-delta_date)
  lacondos[d,]$date = format(temp_date,"%m/%d/%Y")
  #as.Date(lacondos$date,format ="%m/%d/%Y")+months(1)
}


lacondos[,aggresult] = switch(  
  f$scale,  
  "monthly"= months(as.Date(lacondos$date,format = "%m/%d/%Y")),  
  #"daily"= lacondos[,aggresult] <- weekdays(dates$value)
  "quarterly"= quarters(as.Date(lacondos$date,format = "%m/%d/%Y")),  
  #"weekly"= c("Week" = "Week"),
) 

Linear_A_Seasonal_Index <- lacondos[,variable_of_interest] - lacondos$true_CMA
#head(Linear_A_Seasonal_Index,12)
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

Linear_M_Seasonal_Ratios <- lacondos[,variable_of_interest]/lacondos$true_CMA
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

#tail(lacondos,12)

ac_f <- acf(na.omit(lacondos[,1,drop=FALSE]))

if(any(ac_f$acf>significant_threshold))
{
  print("model linear trend")
}

a_model_nonlinear <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="additive")
m_model_nonlinear <- decompose(ts(lacondos[,1,drop=FALSE],frequency=fresult),type="multiplicative")

m_model_nonlinear$random

m_model_nonlinear$x/(m_model_nonlinear$trend*m_model_nonlinear$seasonal)

#nonlinear Trend-Cycle
if (arima_choice == "arima")
{
  ar_decompose_a_nonlinear <- auto.arima(na.omit(matrix(a_model_nonlinear$trend)))
  ar_decompose_m_nonlinear <- auto.arima(na.omit(matrix(m_model_nonlinear$trend)))
}else
  
{
  ar_decompose_a_nonlinear <- arfima(na.omit(matrix(a_model_nonlinear$trend)))
  ar_decompose_m_nonlinear <- arfima(na.omit(matrix(m_model_nonlinear$trend)))
}

#DO NOT USE ARFIMA
if(FALSE)
{
  temp <- fracdiff::fracdiff(na.omit(matrix(a_model_nonlinear$trend)))
  plot(temp$fitted)
  
  forecast(temp)
  t1 <- (ar_decompose_a_nonlinear$fitted)
  t2 <- as.data.frame(forecast(ar_decompose_a_nonlinear)$mean)
  
  colnames(t1) <- "nonlinearCF"
  colnames(t2) <- "nonlinearCF"
  plot(rbind(t1,t2)$nonlinearCF)
}

length(na.omit(a_model_nonlinear$trend))
length(a_model_nonlinear$trend)

sum(a_model_nonlinear$residuals^2)
sum(m_model_nonlinear$residuals^2)

#linear cycle-factor
if (arima_choice == "arima")
{
  ar_CF_M_linear <- auto.arima((((na.omit(lacondos$linear_M_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
  ar_CF_A_linear <- auto.arima((((na.omit(lacondos$linear_A_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
}else
  
{
  ar_CF_M_linear <- arfima((((na.omit(lacondos$linear_M_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
  ar_CF_A_linear <- arfima((((na.omit(lacondos$linear_A_CF)))),trace=FALSE,parallel = TRUE,stepwise=FALSE)
}

#length((na.omit(lacondos$linear_M_CF)))
#length(((lacondos$linear_M_CF)))

plot(na.omit(ar_CF_M_linear$residuals))
acf(na.omit(ar_CF_M_linear$residuals))
pacf(na.omit(ar_CF_M_linear$residuals))

plot(na.omit(ar_CF_A_linear$residuals))
acf(na.omit(ar_CF_A_linear$residuals))
pacf(na.omit(ar_CF_A_linear$residuals))

#
ar_CF_M_linear_forecast <- as.data.frame(forecast(ar_CF_M_linear,h=forecastWindow))
ar_CF_A_linear_forecast <- as.data.frame(forecast(ar_CF_A_linear,h=forecastWindow))

ar_trendcycle_a_nonlinear_forecast <- as.data.frame(forecast(ar_decompose_a_nonlinear,h=forecastWindow))
ar_trendcycle_m_nonlinear_forecast <- as.data.frame(forecast(ar_decompose_m_nonlinear,h=forecastWindow))

#fix months as names
rownames(ar_trendcycle_a_nonlinear_forecast) <- as.integer(rownames(ar_CF_A_linear_forecast))+fresult/2
rownames(ar_trendcycle_m_nonlinear_forecast) <- as.integer(rownames(ar_CF_M_linear_forecast))+fresult/2

rownames(ar_CF_M_linear_forecast) = as.integer(rownames(ar_CF_M_linear_forecast))+fresult/2
rownames(ar_CF_A_linear_forecast) = as.integer(rownames(ar_CF_A_linear_forecast))+fresult/2

colnames(ar_CF_M_linear_forecast) <- paste(colnames(ar_CF_M_linear_forecast), "ar_CF_M_lin_fore", sep = "_")
colnames(ar_CF_A_linear_forecast) <- paste(colnames(ar_CF_A_linear_forecast), "ar_CF_A_lin_fore", sep = "_")
colnames(ar_trendcycle_a_nonlinear_forecast) <- paste(colnames(ar_trendcycle_a_nonlinear_forecast), "ar_CF_A_nonlin_fore", sep = "_")
colnames(ar_trendcycle_m_nonlinear_forecast) <- paste(colnames(ar_trendcycle_m_nonlinear_forecast), "ar_CF_M_nonlin_fore", sep = "_")

temp_df_ <- merge(lacondos, cbind(ar_CF_M_linear_forecast[,c(1,4,5)],ar_CF_A_linear_forecast[,c(1,4,5)],ar_trendcycle_a_nonlinear_forecast[,c(1,4,5)],ar_trendcycle_m_nonlinear_forecast[,c(1,4,5)]), by=0, all=TRUE) 

rownames(temp_df_) <- temp_df_$Row.names

lacondos <- temp_df_[order(as.numeric(row.names(temp_df_))),]

#drop
lacondos = select(lacondos, ,-c("Row.names"))

sum(na.omit(ar_CF_M_linear$residuals)^2)
sum(na.omit(ar_CF_A_linear$residuals)^2)

colnames(lacondos)

#lacondos$forecast_ar_M_linear <- lacondos$CMAT*lacondos$linear_M_CF*lacondos$`Linear_M_Seasonal_Indexes_Adj`
lacondos$forecast_ar_M_linear <- lacondos$CMAT*lacondos$`Point Forecast_ar_CF_M_lin_fore`*lacondos$`Linear_M_Seasonal_Indexes_Adj`
lacondos$forecast_ar_M_linear_lower <- lacondos$CMAT*lacondos$`Lo 95_ar_CF_M_lin_fore`*lacondos$`Linear_M_Seasonal_Indexes_Adj`
lacondos$forecast_ar_M_linear_upper <- lacondos$CMAT*lacondos$`Hi 95_ar_CF_M_lin_fore`*lacondos$`Linear_M_Seasonal_Indexes_Adj`

lacondos$forecast_ar_A_linear <- lacondos$CMAT+lacondos$`Point Forecast_ar_CF_A_lin_fore`+lacondos$`Linear_A_Seasonal_Indexes_Adj`
lacondos$forecast_ar_A_linear_upper <- lacondos$CMAT+lacondos$`Hi 95_ar_CF_A_lin_fore`+lacondos$`Linear_A_Seasonal_Indexes_Adj`
lacondos$forecast_ar_A_linear_lower <- lacondos$CMAT+lacondos$`Lo 95_ar_CF_A_lin_fore`+lacondos$`Linear_A_Seasonal_Indexes_Adj`

#lacondos$forecast_ar_A_nonlinear <- ts(a_model_nonlinear$trend)+ts(a_model_nonlinear$seasonal)+lacondos$`Point Forecast_ar_CF_A_nonlin_fore`
lacondos$forecast_ar_A_nonlinear <- ts(a_model_nonlinear$seasonal)+lacondos$`Point Forecast_ar_CF_A_nonlin_fore`
lacondos$forecast_ar_A_nonlinear_lower <- ts(a_model_nonlinear$seasonal)+lacondos$`Lo 95_ar_CF_A_nonlin_fore`
lacondos$forecast_ar_A_nonlinear_upper <- ts(a_model_nonlinear$seasonal)+lacondos$`Hi 95_ar_CF_A_nonlin_fore`

#lacondos$forecast_ar_M_nonlinear <- ts(m_model_nonlinear$trend)*ts(m_model_nonlinear$seasonal)
lacondos$forecast_ar_M_nonlinear_lower <- ts(m_model_nonlinear$seasonal)*lacondos$`Lo 95_ar_CF_M_nonlin_fore`
lacondos$forecast_ar_M_nonlinear_upper <- ts(m_model_nonlinear$seasonal)*lacondos$`Hi 95_ar_CF_M_nonlin_fore`
lacondos$forecast_ar_M_nonlinear <- ts(m_model_nonlinear$seasonal)*lacondos$`Point Forecast_ar_CF_M_nonlin_fore`

rownames(lacondos) <- as.Date(lacondos$date, format="%m/%d/%Y")
lacondos$date <- rownames(lacondos)

par(mar=c(7, 4, 4, 2) + 0.1, bg="white", cex=1) 
my.dates <- as.Date(rownames(lacondos))
plot(my.dates,lacondos[,variable_of_interest], xlab="", las=1, col="steelblue", pch=20, xaxt="n")
lines(my.dates,lacondos$forecast_ar_M_linear, xlab="", las=1, col="purple", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_M_linear_upper, xlab="", las=1, col="purple", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_M_linear_lower, xlab="", las=1, col="purple", pch=20, xaxt="n",type="p")

lines(my.dates,lacondos$forecast_ar_A_linear, xlab="", las=1, col="red", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_A_linear_upper, xlab="", las=1, col="red", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_A_linear_lower, xlab="", las=1, col="red", pch=20, xaxt="n",type="p")

lines(my.dates,lacondos$forecast_ar_A_nonlinear, xlab="", las=1, col="green", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_A_nonlinear_upper, xlab="", las=1, col="green", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_A_nonlinear_lower, xlab="", las=1, col="green", pch=20, xaxt="n",type="p")

lines(my.dates,lacondos$forecast_ar_M_nonlinear, xlab="", las=1, col="black", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_M_nonlinear_upper, xlab="", las=1, col="black", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_M_nonlinear_lower, xlab="", las=1, col="black", pch=20, xaxt="n",type="p")

axis.Date(1, at=seq(my.dates[1], my.dates[length(my.dates)], "years"),
          labels=seq(my.dates[1], my.dates[length(my.dates)], "years"),
          format= "%Y-%m-%d", las=2)

View(lacondos)