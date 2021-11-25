
lacondos <- read.csv("C:/Users/User/Documents/wiki/wiki/Excel/LACondos.csv",row.names=1)

nrow(lacondos)


training <- nrow(lacondos)*.7
validation <- training-round(nrow(lacondos)*.7*.3,0)
holdout <- nrow(lacondos)-nrow(lacondos)*.7+validation



decompose(ts(lacondos,frequency=4),type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle(colnames(tempdf))