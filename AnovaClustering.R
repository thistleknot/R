#packages
{
  packages_github <- c("UrbanInstitute/urbnmapr","hrbrmstr/hrbrthemes")
  
  installed_packages <- packages_github %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    devtools::install_github(packages_github[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(sub(".*?/", "", packages_github), library, character.only = TRUE))

  packages <- c("dplyr","tidyverse","urbnmapr","hrbrthemes","viridis")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

#finds the convex or invex of any given curve by subtracting the min/max's linear plane and then finding the absolute max.
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


df <- read.csv(file="C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\python\\python-ml\\data\\raw\\states.csv",text=readings, header = TRUE, sep = ",", dec = ".")

data <- df[,2:ncol(df)]

data_scaled <- prcomp(data, center = TRUE, scale. = TRUE)

#f_data <- fviz_nbclust(data_scaled, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")

proportional_scaled_data <- data_scaled$x*summary(data_scaled)$importance[2,]

#cumsum(summary(data_scaled)$importance[2,])

set.seed(Sys.time())

bss_metric <- c()
wss_metric <- c()
for (k in 1:15)
{#k=2
  #print(k)
  km <- kmeans(proportional_scaled_data, nstart=1000, centers=k)
  bss_metric <- c(bss_metric,km$betweenss)
  wss_metric <- c(wss_metric,sum(km$withinss)/length(km$withinss))
}

plot(bss_metric)
plot(wss_metric)

bss_curve = (findknee(bss_metric))
bss_curve = bss_curve[complete.cases(bss_curve)]
bss_curve = bss_curve/max(bss_curve)
wss_curve = (findknee(wss_metric))
wss_curve = wss_curve[complete.cases(wss_curve)]
wss_curve = wss_curve/max(wss_curve)

jpeg(paste("C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\R\\","BSS-WSS",sep = "",".png"),width = 1200, height = 800, units = "px", pointsize = 12, quality = 90)
plot(bss_curve,main="Red: BSS, Blue: WSS", col="red",type="l")
points(bss_curve,main="Red: BSS, Blue: WSS", col="red")
lines(wss_curve,xlab="WSS",col="blue")
points(wss_curve,xlab="WSS",col="blue")
dev.off()

#optimal # of clusters
optimal_k <- which.min(abs(bss_curve/wss_curve-1))

km_ <- kmeans(proportional_scaled_data, nstart=2000, centers=optimal_k)

num = (optimal_k-1)
dem = nrow(proportional_scaled_data)-(optimal_k)

F_scores = (km_$betweenss/num)/(km_$withinss/(dem))

#F_scores[F_scores=="Inf"]=0
F_scores

p_scores = as_data_frame(pf(F_scores, num, dem, lower.tail = FALSE, log.p = FALSE))
colnames(p_scores) <- "p_values-"

View(p_scores)

df$cluster <- km_$cluster

spatial_data <- left_join(get_urbn_map(map = "states", sf = TRUE),
                          statedata,
                          by = "state_name")

spatial_data <- spatial_data[spatial_data$state_name %in% df$States,] %>% left_join(df, by = c('state_name' = 'States'))

spatial_data$cluster <- factor(spatial_data$cluster)

jpeg(paste("C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\R\\","clusterMap",sep = "",".png"),width = 1200, height = 800, units = "px", pointsize = 12, quality = 90)

ggplot() +
  geom_sf(spatial_data,
          mapping = aes(fill = cluster),
          color = "#ffffff", size = 0.25) + scale_fill_discrete() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3)
  labs(fill = "cluster")

dev.off()  

# sample size
sample_size = df %>% group_by(cluster) %>% summarize(num=n())  
  
for (c in colnames(df)[2:(length(colnames(df))-1)])
{
  print(c)
  colnames(sample_size) <- c("name","num")
  
  data <- as_data_frame(cbind(df[,c("cluster"),drop=FALSE],df[,c,drop=FALSE]))
  colnames(data) <- c("name","value")
  
  jpeg(paste("C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\R\\","clusterMap-",c,sep = "",".png"),width = 1200, height = 800, units = "px", pointsize = 12, quality = 90)
  # Plot
  plot(
  data %>%
    left_join(sample_size) %>%
    mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
    ggplot( aes(x=myaxis, y=value, fill=name)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = FALSE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=16)
    ) +
    ggtitle(c) #+
    #xlab("")  + ylab(c)
  )
  dev.off()
}


View(df)
