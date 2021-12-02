#packages
{
  packages_github <- c()
  
  installed_packages <- packages_github %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    devtools::install_github(packages_github[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(sub(".*?/", "", packages_github), library, character.only = TRUE))
  
  packages <- c("dplyr","ggplot2","quantmod","zoo","plyr","forecast","tseries","tidyverse","furrr","lubridate","anytime")
  
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

dates <- as.Date(anytime(rownames(lacondos)))
rownames(lacondos) <- anydate(rownames(lacondos))

f = periodicity(dates)

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

lacondos[,aggresult] = switch(  
  f$scale,  
  "monthly"= months(anydate(rownames(lacondos))),  
  #"daily"= lacondos[,aggresult] <- weekdays(dates$value)
  "quarterly"= quarters(rownames(lacondos)),  
  #"weekly"= c("Week" = "Week"),
) 

decompose_cycle_factor <- function(y,type_,trend_)
{#y=as.data.frame(as.data.frame(na.omit(lacondos$LXXRCSA)),row.names=na.omit(lacondos[,c("LXXRCSA","date")])$date)
  #type_="additive"
  #trend_="linear"
  
  dates <- as_data_frame(as.Date(rownames(y)))
  
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
  
  df <- as.data.frame(y)
  df$date <- rownames(df)
  colnames(df) <- c(variable_of_interest,"date")
  
  df[,"true_CMA"] <- as.data.frame(ma(df[,1], order = fresult, centre=T))
  
  #true_CMA = centered average halving tail months (13 months)
  #CMA is Dr. Zerom's centered average using only 12 months, then 6 months
  '
  true_CMA <- ma(df[,variable_of_interest,drop=FALSE], order = fresult, centre=T)
  
  lacondos$MA_trend_cycle <- lag(zoo::rollmean(lacondos[,variable_of_interest,drop=FALSE], k = fresult, fill = NA, align="center"), n=1)
  lacondos$CMA <- zoo::rollmean(lacondos$MA_trend_cycle, k = fresult/2, fill = NA, align="left")
  lacondos$time <- 1:nrow(lacondos)
  
  class(lacondos$true_CMA) <- class(lacondos$CMA)
  
  CMA_df <- lacondos[,c("time","true_CMA")][complete.cases(lacondos[,c("time","true_CMA")]),]
  
  lacondos$date <- rownames(lacondos)
  '
  
  #initial_decompose <- decompose(ts(y,frequency=fresult),type=type_)
  
  #lacondos$Date <- dates$value
  
  #CMAT
  if (trend_=="linear")
  {
   
    CMA_df_lm <- lm(df[,"true_CMA",drop=TRUE] ~ as.integer(rownames(as.data.frame(df[,1])))) 
    
    df[,"CMAT"] <- data.frame(1:(nrow(as.data.frame(y)))*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1])
    
  } else
    if (trend_model == "poly") {
      
      y_ <- df[,"true_CMA",drop=FALSE]
      colnames(y_) <- "true_CMA"
      
      q <- poly(index(df[,"date",drop=TRUE]),porder,raw=TRUE)
      
      #newq <- poly(1:(nrow(lacondos)+forecastWindow),porder)
      
      CMA_df_lm <- lm(y_[,1]~.,data=as.data.frame(cbind(df[,"true_CMA",drop=FALSE],q)))
      
      #poly(1:(nrow(lacondos)+forecastWindow),porder)
      
      # <- #predict(CMA_df_lm,data=index(df[,"date",drop=TRUE]),type="response")#poly(1:(nrow(lacondos)+forecastWindow),porder)
      
      temp_df <- as.data.frame(predict(CMA_df_lm,data=index(df[,"date",drop=TRUE]),type="response"))
      
      colnames(temp_df) <- "CMAT"
      #View(temp_df)
      df[,"CMAT"] <- merge(df, temp_df, by=0, all=TRUE) %>% select(CMAT)

      
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
  
  #rownames(lacondos) <- lacondos$time
  
  #lacondos <- select(lacondos, ,-c("time"))
  
  #inputs preceding 0 before months...
  for (d in ((nrow(lacondos)+1)-(fresult*3)):nrow(lacondos))
  {#d=nrow(lacondos)
    delta_date <- ((nrow(lacondos))-(fresult*3))
    
    temp_date <- as.Date(lacondos[(nrow(lacondos)-(fresult*3)),]$date,format = "%m/%d/%Y")+months(d-delta_date)
    lacondos[d,]$date = format(temp_date,"%m/%d/%Y")
    #as.Date(lacondos$date,format ="%m/%d/%Y")+months(1)
  }
  
  if(type_=="additive")
  {
    df$CF <- as.data.frame(df$true_CMA-df$CMAT)[,1]
    df$SI <- as.data.frame(df[,1] - df$true_CMA)[,1]
  } else
  {
    df$CF <- as.data.frame(df$true_CMA/df$CMAT)[,1]
    df$SI <- as.data.frame(df[,1] / df$true_CMA)[,1]
  }
  
  temp_ <- switch(  
    f$scale,  
    "monthly"= months(as.Date(df$date)),
    #"daily"= lacondos[,aggresult] <- weekdays(dates$value)
    "quarterly"= quarters(as.Date(df$date)),  
    #"weekly"= c("Week" = "Week"),
  ) 
  
  df[,aggresult] = temp_
  
  if(type_=="additive")
  {
    Linear_Seasonal_Index <- df[,1,drop=FALSE] - df[,"true_CMA",drop=FALSE]
  }else
  {
    Linear_Seasonal_Index <- df[,1,drop=FALSE] / df[,"true_CMA",drop=FALSE]
  }
  colnames(Linear_Seasonal_Index) <- "Linear_Seasonal_Index"
  Linear_Seasonal_Index$Month <- df$Month
  
  Linear_Seasonal_Index <- cbind(Linear_Seasonal_Index,df[,1,drop=FALSE])
  
  f1 <- as.formula(paste("Linear_Seasonal_Index", " ~ ", aggresult))
  
  Linear_Seasonal_Indexes <- aggregate(f1, na.omit(Linear_Seasonal_Index) , mean)
  
  if(type_=="additive")
  {
    Linear_Seasonal_Indexes_Adj <- Linear_Seasonal_Indexes[,"Linear_Seasonal_Index",drop=FALSE]-mean(Linear_Seasonal_Indexes[,"Linear_Seasonal_Index"])
    
  }else
  {
    Linear_Seasonal_Indexes_Adj <- Linear_Seasonal_Indexes[,"Linear_Seasonal_Index",drop=FALSE]/mean(Linear_Seasonal_Indexes[,"Linear_Seasonal_Index"])
  }
  
  colnames(Linear_Seasonal_Indexes_Adj) <- "Linear_Seasonal_Indexes_Adj"
  rownames(Linear_Seasonal_Indexes_Adj) <- Linear_Seasonal_Indexes[,aggresult]
  
  Linear_Seasonal_Indexes$Linear_Seasonal_Indexes_Adj = Linear_Seasonal_Indexes_Adj
  
  Linear_Seasonal_Indexes_Adj[,aggresult] = Linear_Seasonal_Indexes[,aggresult]
  
  df$SI_Adj <- left_join(df,Linear_Seasonal_Indexes_Adj, by = aset) %>% select(Linear_Seasonal_Indexes_Adj)
  
  CF_fore <- as.data.frame(forecast(auto.arima(df$CF),h = (fresult+fresult/2)))
  
  if(fresult == 4)
  {
    rownames(CF_fore) <- head(as.Date(tail(rownames(na.omit(df[,"CF",drop=FALSE])),1))+months((1:(nrow(CF_fore)))*(fresult-1)),nrow(CF_fore))
  }else{
    rownames(CF_fore) <- head(as.Date(tail(rownames(na.omit(df[,"CF",drop=FALSE])),1))+months((1:(nrow(CF_fore)))),nrow(CF_fore))
  }
  
  CF_fore$date <- rownames(CF_fore)
  CF_fore$Month <- months(as.Date(CF_fore$date))
  
  CF_fore$SI <- left_join(CF_fore,Linear_Seasonal_Indexes_Adj, by = aset) %>% select(Linear_Seasonal_Indexes_Adj)
  
  CF_fore[,"CMAT"] <- data.frame((as.integer(tail(rownames(na.omit(tail(as.data.frame(df$CF),fresult))),1))+1):((as.integer(tail(rownames(na.omit(tail(as.data.frame(df$CF),fresult))),1))+1)+((nrow(CF_fore)))-1)*CMA_df_lm$coefficients[2]+CMA_df_lm$coefficients[1])
  
  if(type_=="additive")
  {
    fore_ <- cbind(CF_fore[,1,drop=FALSE]+CF_fore[,"SI",drop=FALSE]+CF_fore[,"CMAT",drop=FALSE],CF_fore[,2,drop=FALSE]+CF_fore[,"SI",drop=FALSE]+CF_fore[,"CMAT",drop=FALSE],CF_fore[,3,drop=FALSE]+CF_fore[,"SI",drop=FALSE]+CF_fore[,"CMAT",drop=FALSE],CF_fore[,4,drop=FALSE]+CF_fore[,"SI",drop=FALSE]+CF_fore[,"CMAT",drop=FALSE],CF_fore[,5,drop=FALSE]+CF_fore[,"SI",drop=FALSE]+CF_fore[,"CMAT",drop=FALSE])
    
  } else
  {
    fore_ <- cbind(CF_fore[,1,drop=FALSE]*CF_fore[,"SI",drop=FALSE]*CF_fore[,"CMAT",drop=FALSE],CF_fore[,2,drop=FALSE]*CF_fore[,"SI",drop=FALSE]*CF_fore[,"CMAT",drop=FALSE],CF_fore[,3,drop=FALSE]*CF_fore[,"SI",drop=FALSE]*CF_fore[,"CMAT",drop=FALSE],CF_fore[,4,drop=FALSE]*CF_fore[,"SI",drop=FALSE]*CF_fore[,"CMAT",drop=FALSE],CF_fore[,5,drop=FALSE]*CF_fore[,"SI",drop=FALSE]*CF_fore[,"CMAT",drop=FALSE])
  }
  
  colnames(fore_) <- colnames(CF_fore[,1:5])
  return(tail(fore_,-(fresult/2)))
}
  
lacondos$date <- rownames(lacondos)

#data_ = as.data.frame(ts_data,row.names=as.Date(as.yearqtr(index(ts_data), format = "Q%q/%y"), frac = 0))
data_ = as.data.frame(as.data.frame(na.omit(lacondos$LXXRCSA)),row.names=na.omit(lacondos[,c("LXXRCSA","date")])$date)

forecast_ <- decompose_cycle_factor(data_,"multiplicative","linear")

dm <- as.data.frame(cbind(as.Date(rownames(forecast_, "%m/%d/%Y")),forecast_[,1]))
colnames(dm) <- c("date","forecast")
dm$date <- as.Date(dm$date)
plot(dm)

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

ts_data <- ts(aggregate.ts(ts(na.omit(lacondos[,variable_of_interest,drop=FALSE]),frequency=fresult),mean, nfrequency = 4),start=c(year(lacondos$date[1]),quarter(lacondos$date[1])),frequency=4)

f_ses  <- function(y, h) ses(y,alpha = NULL,h=h,initial="simple")
f_auto_arima  <- function(y, h) forecast(auto.arima(y), h)
f_decompose_a  <- function(y, h, f) forecast(decompose(y,frequency=f,type="additive"), h)
f_decompose_m  <- function(y, h) forecast(auto.arima(y), h)
f_arfima  <- function(y, h) forecast(arfima(y,estim = c("ls")), h)
f_ets <- function(y, h) predict(ets(y), h)

as.data.frame(ts(aggregate.ts(ts(na.omit(lacondos[,variable_of_interest,drop=FALSE]),frequency=fresult),mean, nfrequency = 4),start=c(year(lacondos$date[1]),quarter(lacondos$date[1])),frequency=4))

as.data.frame(ts(aggregate.ts(ts(na.omit(lacondos[,variable_of_interest,drop=FALSE]),frequency=fresult),mean, nfrequency = 4),start=c(year(lacondos$date[1]),quarter(lacondos$date[1])),frequency=4))

#5 CV folds
window_ = ceiling(nrow(matrix(ts_data))/5*4)
h_ = fresult

models_tsCV = list(
  tsCV(ts_data, naive, h=h_, window = window_),
  tsCV(ts_data, f_ses, h=h_, window = window_),
  tsCV(ts_data, holt, h=h_, window = window_),
  tsCV(ts_data, holt, damped = TRUE, h = h_, window = window_),
  tsCV(ts_data, hw, seasonal = "additive", h=h_, window = window_),
  tsCV(ts_data, hw, seasonal = "additive", damped=TRUE, h=h_, window = window_),
  tsCV(ts_data, hw, seasonal = "multiplicative", h=h_, window = window_),
  tsCV(ts_data, hw, seasonal = "multiplicative", damped=TRUE, h=h_, window = window_),
  tsCV(ts_data, f_auto_arima, h=h_, window = window_),
  tsCV(ts_data, f_arfima, h=1,window = window_),
  tsCV(ts_data, f_ets, h=h_, window = window_)
)

errors_tsCV = array(0,dim=c(length(models_tsCV),1))

c_ = 1
for (m in models_tsCV)
{
  errors_tsCV[c_] = mean(m^2, na.rm = TRUE)
  c_ = c_ + 1
}

print(errors_tsCV)

models = list(
  naive(ts_data, h=h_),
  f_ses <- ses(ts_data,h=h_),
  holt(ts_data,h=h_),
  holt(ts_data, damped=TRUE,h=h_),
  hw(ts_data, seasonal = "additive",h=h_),
  hw(ts_data, seasonal = "additive", damped=TRUE,h=h_),
  hw(ts_data, seasonal = "multiplicative",h=h_),
  hw(ts_data, seasonal = "multiplicative", damped=TRUE,h=h_),
  f_auto_arima(ts_data, h=h_),
  f_arfima(ts_data, h=h_),
  f_ets(ts_data,h=h_)
)

model_scores <- array(0,dim=c(length(models),8))

for (m in 1:length(models))
{
  print(m)
  model_scores[m,1] = if(is.null(models[m][[1]]$method)){"arima"}else{models[m][[1]]$method}
  model_scores[m,2:8] = c(accuracy(models[m][[1]]))
}

colnames(model_scores) <- c("name","ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")
#View(model_scores)


models[[which.min(errors_tsCV)]] -> fc1
models[[1]] -> fc2

fc1 %>% forecast(h = fresult) %>% autoplot() +
  autolayer(fitted(fc1), series="Fitted") +
  autolayer(ts_data, series="Data")
  
'
autoplot(ts_data, series="Data") +
  autolayer(fitted(fc1), series="Fitted")
'

#winters_m <- HoltWinters(ts(na.omit(lacondos[,variable_of_interest,drop=TRUE]),frequency = fresult),seasonal=c("multiplicative"))

par(mar=c(7, 4, 4, 2) + 0.1, bg="white", cex=1) 
my.dates <- as.Date(rownames(lacondos))
plot(my.dates,lacondos[,variable_of_interest], xlab="", las=1, col="steelblue", pch=20, xaxt="n")

lines(my.dates,lacondos$forecast_ar_M_linear, xlab="", las=1, col="purple", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_M_linear_upper, xlab="", las=1, col="purple", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_M_linear_lower, xlab="", las=1, col="purple", pch=20, xaxt="n",type="p")

lines(my.dates,lacondos$forecast_ar_A_linear, xlab="", las=1, col="red", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_A_linear_upper, xlab="", las=1, col="red", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_A_linear_lower, xlab="", las=1, col="red", pch=20, xaxt="n",type="p")
'
lines(my.dates,lacondos$forecast_ar_A_nonlinear, xlab="", las=1, col="green", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_A_nonlinear_upper, xlab="", las=1, col="green", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_A_nonlinear_lower, xlab="", las=1, col="green", pch=20, xaxt="n",type="p")

lines(my.dates,lacondos$forecast_ar_M_nonlinear, xlab="", las=1, col="black", pch=20, xaxt="n",type="l")
lines(my.dates,lacondos$forecast_ar_M_nonlinear_upper, xlab="", las=1, col="black", pch=20, xaxt="n",type="p")
lines(my.dates,lacondos$forecast_ar_M_nonlinear_lower, xlab="", las=1, col="black", pch=20, xaxt="n",type="p")
'
axis.Date(1, at=seq(my.dates[1], my.dates[length(my.dates)], "years"),
          labels=seq(my.dates[1], my.dates[length(my.dates)], "years"),
          format= "%Y-%m-%d", las=2)

View(lacondos)