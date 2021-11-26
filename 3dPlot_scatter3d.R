#library("car")
#library("rgl")

#data(iris)

#sep.l <- iris$Sepal.Length
#sep.w <- iris$Sepal.Width
#pet.l <- iris$Petal.Length

#scatter3d(x = sep.l, y = pet.l, z = sep.w, surface=FALSE, labels = rownames(iris), id.n=nrow(iris))

test2 <- cbind(dataSet[,paste(d)],set.final$Groups,test)
#X <- dataSet[,paste(d[1])]
#Y <- dataSet[,"Poverty"]
#Z <- dataSet[,paste(d[2])]


# 3D plot with the regression plane

scatter3d(x = X, y = Y, z = Z, groups = NULL, #test2$`set.final$Groups`,
          grid = FALSE, fit=c("linear"),ellipsoid = TRUE, surface=TRUE,
          surface.col = c("green", "blue", "red"), residuals=TRUE,
          id=list(n = length(test2$test), labels = test2$test, parallel=FALSE)
          
)
rglwidget()
Sys.sleep(5)

scatter3d(x = X, y = Y, z = Z, groups = test2$`set.final$Groups`,
          grid = FALSE, fit=c("linear"),ellipsoid = TRUE, surface=TRUE,
          surface.col = c("green", "blue", "red"), residuals=TRUE,
          id=list(n = length(test2$test), labels = test2$test, parallel=FALSE)
          
)
rglwidget()
Sys.sleep(5)

X <- dataSet[,paste(d[3])]
Y <- dataSet[,paste(d[1])]
Z <- dataSet[,paste(d[2])]

scatter3d(x = X, y = Y, z = Z, groups = test2$`set.final$Groups`,
          grid = FALSE, fit=c("linear"),ellipsoid = TRUE, surface=TRUE,
          surface.col = c("green", "blue", "red"), residuals=TRUE,
          id=list(n = length(test2$test), labels = test2$test, parallel=FALSE
                  #, 
                  #model.summary=TRUE
          )
          
)
rglwidget()
Sys.sleep(5)

group <- quantcut(studres(finalModelPCA),3)
X <- cooks.distance(finalModelPCA)
Y <- hatvalues(finalModelPCA)
z <- dffits(finalModelPCA)

scatter3d(x = X, y = Y, z = Z, groups = group,
          grid = FALSE, fit=c("linear"),ellipsoid = FALSE, surface=FALSE,
          surface.col = c("green", "blue", "red"), residuals=TRUE,
          id=list(n = length(test2$test), labels = test2$test, parallel=FALSE
                  #, 
                  #model.summary=TRUE
          )
          
)

rglwidget()

fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig