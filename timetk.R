#https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92
#
#plot(df[c("Date","MSPUS")])
packages <- c("timetk","dplyr","tidyverse","factoextra","NbClust","xts","magrittr","clustree","magick","reshape")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#

my_mean <- function(x, na.rm=TRUE) {
  mean(x, na.rm = na.rm)
}

df <- read.csv(file="C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\python\\python-stock\\data\\combined_set.csv",header = TRUE, sep = ",", dec = ".")
df$Date = as.Date(df$Date,origin=df$Date[1])

tempdf <- df[,"MSPUS",drop=FALSE]
rownames(tempdf) <- df$Date

#as_tibble(df)
tibble_df <- as_tibble(df)

tibble_df$Date <- as.Date(tibble_df$Date)

#reshape(df, idvar = "Date", varying = names(df$Date))

#df[df$]
melted = c()

for (c in colnames(df)[2:ncol(df)])
{
  subset = df[, c("Date",c)]
  subset$variable = c
  colnames(subset) <- c("Date","value","variable")
  #
  melted = rbind(melted,subset)
  #print(head(subset))
  #print(subset)
  
}
head(melted)
melted$Date <- as.Date(melted$Date)

tsfeature_tbl <- as_tibble(melted) %>%
  group_by(variable) %>%
  tk_tsfeatures(
    .date_var = Date,
    .value    = value,
    .period   = 4,
    .features = c("frequency", "stl_features", "entropy", "acf_features", "my_mean"),
    .scale    = TRUE,
    .prefix   = "ts_"
  ) %>%
  ungroup()

print(tsfeature_tbl)

#class(tsfeature_tbl$ts_frequency)

removeZeroVar3 <- function(df){
  df[, !sapply(df, function(x) min(x) == max(x))]
}

relevant_tsfeature_tbl <- removeZeroVar3(tsfeature_tbl[!names(tsfeature_tbl) %in% c("variable","ts_diff2_acf1","ts_my_mean","ts_spike")])

relevant_tsfeature_tbl_pca <- prcomp(relevant_tsfeature_tbl, center = TRUE, scale. = TRUE)

selectedPCs = names(cumsum(summary(relevant_tsfeature_tbl_pca)$importance[2,])[cumsum(summary(relevant_tsfeature_tbl_pca)$importance[2,])<=.99])

pca_df = relevant_tsfeature_tbl_pca$x[,selectedPCs]*summary(relevant_tsfeature_tbl_pca)$importance[2,selectedPCs]
#relevant_tsfeature_tbl_pca$x[,selectedPCs]*summary(relevant_tsfeature_tbl_pca)$importance[2,selectedPCs]

metric1 <- c()
metric2 <- c()
for (k in 1:50)
{#k=2
  #print(k)
  km <- kmeans(pca_df, nstart=100, centers=k)
  metric1 <- c(metric1,km$betweenss)
  metric2 <- c(metric2,sum(km$withinss)/length(km$withinss))
}

plot(metric1)
plot(metric2)

findknee <- function(xdata)
{
  rate_of_change=(xdata[1]-xdata[length(xdata)])/(length(xdata)-1)
  xdata$delta = xdata-xdata[length(xdata)]
  xdata$deltas[1] = xdata$delta[1]
  for (d in 2:length(xdata))
  {
    xdata$deltas[d]=xdata$deltas[d-1]-rate_of_change
  }
  for (d in 1:length(xdata))
  {
    xdata$deltas[d]=xdata$delta[d]-xdata$deltas[d]
  }
  return(abs(xdata$deltas))
}

knee1 = (findknee(metric1))
knee1 = knee1[complete.cases(knee1)]
knee1 = knee1/max(knee1)
knee2 = (findknee(metric2))
knee2 = knee2[complete.cases(knee2)]
knee2 = knee2/max(knee2)

which(knee1/knee2==1)

print(which.max(knee1))
print(which.max(knee2))

plot(knee1)
lines(knee1)
lines(knee2)

#View(relevant_tsfeature_tbl)
{
  gap_stat <- fviz_nbclust(pca_df, kmeans, method = c("gap_stat"),nboot=500)
  #gap formula
  gap_check = gap_stat$data$gap[1:length(gap_stat)-1]-gap_stat$data$gap[2:length(gap_stat)]+gap_stat$data$SE.sim[2:length(gap_stat)]
  best_gap = min(which(gap_check>0))
  
  gap_kmeans = kmeans(as_data_frame(pca_df), best_gap, nstart = 1000)
  
  F_gap_kmeans_numerator = best_gap-1
  F_gap_kmeans_denominator = (nrow(pca_df)-best_gap)
  
  F_gap_kmeans = (gap_kmeans$betweenss/F_gap_kmeans_numerator)/(gap_kmeans$tot.withinss/F_gap_kmeans_denominator)
  
  F_gap_kmeans_p_score = pf(F_gap_kmeans, F_gap_kmeans_numerator, F_gap_kmeans_denominator, lower.tail = FALSE, log.p = FALSE)
}
best_gap

{
  silh <- fviz_nbclust(pca_df, kmeans, method = c("silhouette"),nboot=100)
  
  best_silh = which.max(silh$data$y)
  
  F_silh_kmeans_numerator = best_silh-1
  F_silh_kmeans_denominator = (nrow(pca_df)-best_silh)
  
  best_silh_kmeans = kmeans(as_data_frame(pca_df), best_silh, nstart = 1000)
  F_silh_kmeans = (best_silh_kmeans$betweenss/F_silh_kmeans_numerator)/(best_silh_kmeans$tot.withinss/F_silh_kmeans_denominator)
  
  best_silh_kmeans_numerator = best_silh-1
  best_silh_kmeans_denominator = (nrow(pca_df)-best_silh)
  
  best_silh_kmeans_p_score = pf(F_gap_kmeans, F_silh_kmeans_numerator, F_silh_kmeans_denominator, lower.tail = FALSE, log.p = FALSE)
}
best_silh


{
  wss_s <- fviz_nbclust(pca_df, kmeans, method = c("wss"),nboot=100)
  
  rate_of_change=(wss_s$data$y[1]-wss_s$data$y[length(wss_s$data$y)])/(length(wss_s$data$y)-1)
  
  wss_s$data$delta = wss_s$data$y-wss_s$data$y[length(wss_s$data$y)]
  wss_s$data$deltas[1] = wss_s$data$delta[1]
  for (d in 2:length(wss_s$data$y))
  {
    wss_s$data$deltas[d]=wss_s$data$deltas[d-1]-rate_of_change
  }
  for (d in 1:length(wss_s$data$y))
  {
    wss_s$data$deltas[d]=wss_s$data$delta[d]-wss_s$data$deltas[d]
  }
  plot(abs(wss_s$data$deltas))
    
  knee = which.max(abs(wss_s$data$deltas))
  
}
knee

res<-NbClust(pca_df, distance = "euclidean", min.nc=2, max.nc=15, 
             method = "ward.D", index = "all")

res$All.index
res$Best.nc
res$Best.nc[1,]
res$All.CriticalValues
res$Best.partition

res$All.CriticalValues

{
  tmp <- NULL
  for (k in 1:15){
    tmp[k] <- kmeans(pca_df, k, nstart = 1000)
  }
  df_ <- data.frame(tmp)
  # add a prefix to the column names
  colnames(df_) <- seq(1:15)
  
  colnames(df_) <- paste0("k",colnames(df_))
  # get individual PCA
  df.pca <- prcomp(df_, center = TRUE, scale. = FALSE)
  
  ind.coord <- df_.pca$x
  #ind.coord <- ind.coord[,1:3,drop=FALSE]
  
  df_ <- bind_cols(df_, as.data.frame(ind.coord))
  clustree(df_, prefix = "k")
}

set.seed(123)

pca_df <- cbind(as_data_frame(pca_df),tsfeature_tbl$variable)

relevant_tsfeature_tbl$variable = tsfeature_tbl$variable

colnames(pca_df) = c(colnames(pca_df)[1:ncol(pca_df)-1],"variable")  

cluster_tbl <- tibble(
  cluster = pca_df %>% 
    select(-variable) %>%
    as.matrix() %>%
    kmeans(centers = max(res$Best.partition), nstart = 1000) %>%
    pluck("cluster")
) %>%
  bind_cols(
    tsfeature_tbl
  )

cluster_tbl

#View(cluster_tbl)


#
for (c in 1:max(res$Best.partition))
{
  print(c)
  data = cluster_tbl[cluster_tbl$cluster==c,]
  print(data$variable)
  
  random_sample = melted[melted$variable==sample(data$variable,3),]
  
  #fig <- image_graph(width = 1920, height = 1200, res = 300)
  jpeg(paste("C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\R\\","clusterFinanceTS",c,sep = "",".png"),width = 1200, height = 800, units = "px", pointsize = 12, quality = 90)
  plot(data %>%
    select(cluster, variable) %>%
    right_join(random_sample, by = "variable") %>%
    group_by(variable) %>%
    plot_time_series(
      Date, value, 
      .color_var   = cluster, 
      .smooth=TRUE,
      .facet_ncol  = 1, 
      .interactive = FALSE
    ))
  dev.off()
  #print(fig)
  #print(nrow(cluster_tbl[cluster_tbl$cluster==c,]))
}



finalk <- kmeans(pca_df[,1:ncol(pca_df)-1], centers = max(res$Best.partition), nstart = 100)

finalk$cluster <- res$Best.partition

p1 <- fviz_cluster(finalk, data = pca_df[,1:ncol(pca_df)-1], frame.type = "convex") + theme_minimal() + ggtitle("k = 2")

p1

