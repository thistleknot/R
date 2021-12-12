#https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92
df <- read.csv(file="C:\\Users\\User\\Documents\\wiki\\wiki\\dev\\python\\python-ml\\data\\raw\\states.csv",text=readings, header = TRUE, sep = ",", dec = ".")

packages <- c("tidyverse","magrittr","cluster","cluster.datasets","cowplot","NbClust","clValid","ggfortify","clustree","gen3DNet","KneeArrower","dendextend","factoextra","FactoMineR","corrplot","GGally","ggiraphExtra","knitr","kableExtra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

data <- df[,2:ncol(df)]

corrplot(cor(data), type = "upper", method = "ellipse", tl.cex = 0.9)

data_scaled <- scale(data)
rownames(data_scaled) <- df$States

res.pca <- PCA(data_scaled,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle

km2 <- kmeans(data_scaled, centers = 2, nstart = 30)

kmean_calc <- function(df, ...){
  kmeans(df, scaled = ..., nstart = 30)
}
km2 <- kmean_calc(data_scaled, 2)
km3 <- kmean_calc(data_scaled, 3)
km4 <- kmeans(data_scaled, 4)
km5 <- kmeans(data_scaled, 5)
km6 <- kmeans(data_scaled, 6)
km7 <- kmeans(data_scaled, 7)
km8 <- kmeans(data_scaled, 8)
km9 <- kmeans(data_scaled, 9)
km10 <- kmeans(data_scaled, 10)
km11 <- kmeans(data_scaled, 11)
p1 <- fviz_cluster(km2, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 2") 
p2 <- fviz_cluster(km3, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 3")
p3 <- fviz_cluster(km4, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 4")
p4 <- fviz_cluster(km5, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 5")
p5 <- fviz_cluster(km6, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 6")
p6 <- fviz_cluster(km7, data = data_scaled, frame.type = "convex") + theme_minimal() + ggtitle("k = 7")
plot_grid(p1, p2, p3, p4, p5, p6, labels = c("k2", "k3", "k4", "k5", "k6", "k7"))

set.seed(31)
# function to compute total within-cluster sum of squares
f_data <- fviz_nbclust(data_scaled, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")

rate_of_change=(f_data$data$y[1]-f_data$data$y[length(f_data$data$y)])/(length(f_data$data$y)-1)

f_data$data$delta = f_data$data$y-f_data$data$y[length(f_data$data$y)]
f_data$data$deltas[1] = f_data$data$delta[1]
for (d in 2:length(f_data$data$y))
{
  f_data$data$deltas[d]=f_data$data$deltas[d-1]-rate_of_change
  
}
for (d in 1:length(f_data$data$y))
{
  f_data$data$deltas[d]=f_data$data$delta[d]-f_data$data$deltas[d]
}
plot(abs(f_data$data$deltas))

knee = which.max(abs(f_data$data$deltas))

print(knee)