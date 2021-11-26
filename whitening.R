library(whitening)
library(DMwR)
library(bnstruct)

source("functions.R")

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]

#log transform crime and population due to mc producing negative results (and comparing log transform of original vars looks normal)
data2 <- cbind(data2[,1:2],(data2[,3,drop=FALSE]^5),(data2[,4,drop=FALSE]^(1/3)),data2[,5:9],log(data2[,10,drop=FALSE]))

rownames(data2) <- data[,1]

cov(data2)
covMatrix(data2)

#scaledData (important to scale, else results don't converge on whiten)
sd <- scale(data2,center=TRUE,scale=TRUE)

#covariance Matrix
cm <- covMatrix(sd)
cm
#same as cov(sd)
cov(sd)

e <- eigen(covMatrix(scale(data2,center=TRUE,scale=TRUE)))

evecs <- e$vectors
evals <- e$values

#https://cbrnr.github.io/2018/12/17/whitening-pca-zca/

#whiten Matrix (same as whiteningMatrix)
wm <- evecs %*% diag(evals^(-1/2)) %*% t(evecs)
wm
whitening::whiteningMatrix(cm, method=c("ZCA"))

#wd <- tcrossprod(as.matrix(scale(data2),center=TRUE), wm)
#whitened data
wd <- as.matrix(sd) %*% wm
wd
whiten(as.matrix(sd), method=c("ZCA"))

#walk back

#scaled <- tcrossprod(wd,inv(wm))
scaled <- wd %*% t(inv(wm))

#apply mean and original sdev
unscale(scaled,sd)

#made up data (monte carlo)

wd2 <- matrix(rnorm(1000),100,10)
scaled2 <- wd2 %*% t(inv(wm))
data3 <- unscale(scaled2,sd)

#some of the data for white % is messed up
nd <- cbind(data3[,1:2],data3[,3,drop=FALSE]^(1/5),(data3[,4,drop=FALSE]^3),data3[,5:9],exp(data3[,10,drop=FALSE]))

colnames(nd) <- colnames(data2)
nd[is.nan(nd)] <- NA

nd <- knn.impute(nd, k = 14, cat.var = 1:ncol(nd), to.impute = 1:nrow(nd),using = 1:nrow(nd))
summary(nd)

df <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(data2[,-1]),method=c("ZCA"),center=TRUE))
colnames(df) <- c(colnames(data2[,1,drop=FALSE]),colnames(data2[,-1]))
model <- lm(data.frame(df))
summary(model)

df <- dplyr::select(data2[,-1], -c('University'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
summary(model)

df <- dplyr::select(data2[,-1], -c('University','Population'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
round(sm$coefficients[,1],3)

df <- dplyr::select(data2[,-1], -c('University','Population','Crime'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
round(sm$coefficients[,1],3)

#model goes down, but all significant terms
df <- dplyr::select(data2[,-1], -c('University','Population','Crime','Doctors'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
cvalues <- sort(abs(round(sm$coefficients[,1],3))[-1])

#model goes down, but all significant terms
df <- dplyr::select(data2[,-1], -c('University','Population','Crime','Doctors','Traf.Deaths'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
cvalues <- sort(abs(round(sm$coefficients[,1],3))[-1])

#model goes down, but all significant terms
df <- dplyr::select(data2[,-1], -c('University','Population','Crime','Doctors','Traf.Deaths','Infant.Mort'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
cvalues <- sort(abs(round(sm$coefficients[,1],3))[-1])

#So with crazy transformations.  I end up with White, Unemployed, Income and a .85 Adj R^2

df <- dplyr::select(data[,-1], -c('State','University','Population','Crime','Doctors','Traf.Deaths','Infant.Mort'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
cvalues <- sort(abs(round(sm$coefficients[,1],3))[-1])

pairs.panels(df2,method = "pearson", # correlation method
             pch=21,            
             density = TRUE,  # show density plots
             ellipses = TRUE#, # show correlation ellipses
             #bg=c("red","yellow","blue","purple")
)	


#dropping one pca vs population has a negligable effect (.03)
#all significant with PCA.  Removing last PCA has a dramatic affect on Adj R^2
df <- dplyr::select(data[,-1], -c('State','University','Population','Crime','Doctors','Traf.Deaths','Infant.Mort'))
pca.set <- prcomp(as.matrix(df),scale=TRUE,center=TRUE)
summary(pca.set)
dfPC <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),pca.set$x[,1:3])
#colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
pcamodel <- lm(data.frame(dfPC))
pcasm <- summary(pcamodel)
pcasm
#cvalues <- sort(abs(round(sm$coefficients[,1],3))[-1])

#https://blogs.sas.com/content/iml/2010/12/10/converting-between-correlation-and-covariance-matrices.html

plot(pcamodel)
hist(pcamodel$residuals)

hcolors=rainbow(16, start=0, end=1)[quantcut(df2[,1],3)]
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

bg3d("white")

pca3d(pca.set, group = hcolors , show.scale=TRUE, show.plane = FALSE, show.labels = states ,show.centroids = TRUE,show.ellipses=FALSE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()

data_plot <- plot_ly(data=data.frame(df2[,c("Income","White","Unemployed")]),
                     y = ~White,
                     x = ~Unemployed,
                     z = ~Income,
                     text = states, #mydata$state, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors
                                   #,symbol=hsymbols
                     ))

data_plot

sdf <- sd(df2)
cdf <- cor(df2)

cor2cov(cdf,sdf)

library(corrplot)
corrplot(cor(df2))

