

library("Rcmdr")
library("scatterplot3d")

#simple
attach(data)
scatterplot3d(Population, Income, Unemployed, main="3D Scatterplot")

s3d <- scatterplot3d(Poverty, Population, Income, main="3D Scatterplot", pch=16, highlight.3d=TRUE, type="h")

fit <- lm(Poverty ~ Population+Income, data=data) 

s3d$plane3d(fit)

mycolors <- c('black', 'red', 'blue')
data$color <- mycolors[ as.numeric(set.final$Groups) ]

# 3D scatter plot
s3d <- scatterplot3d(trees, type = "h", color = "blue",
                     angle=55, pch = 16)
# Add regression plane
my.lm <- lm(data$Poverty ~ data$Population+data$Income)

s3d$plane3d(my.lm)
# Add supplementary points

s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)

par(mar=c(0,0,0,0))

with(dataSet,plot3d( 
  x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],], 
  col = data$color, 
  type = 's', 
  size = .2,
  radius = .2,
  xlab=paste(d[1]), ylab=paste(d[2]), zlab=paste(d[3]))
)
with(dataSet,text3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],test))


library(rgl)
rgl.init()
#rgl.spheres(x, y, z, r = 0.2, color = "#D95F02") 
#rgl_add_axes(x, y, z, show.bbox = FALSE)

library(car)

scatter3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],surface=FALSE,axis.scales=TRUE)
texts3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],test)
texts3d(x = dat$PC1, y = dat$PC2, z = dat$PC3,
        groups = dat$group,texts=dat$group, pos=2,color=mycolorscheme,adj=0)

Identify3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],axis.scales=TRUE)
Identify3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],], axis.scales=TRUE, groups = set.final$Groups, labels = test,
           col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
           offset = ((100/length(x))^(1/3)) * 0.02)

mycolors <- c('black', 'red', 'blue')
data$color <- mycolors[ as.numeric(set.final$Groups) ]

open3d()
scatter3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],
          groups = set.final$Groups,
          grid = T, surface = T,ellipsoid = T,surface.col =mycolors,
          xlab="PC1",ylab="PC2",zlab="PC3")
texts3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],
        groups = set.final$Groups,texts=test, pos=2,color=mycolors,adj=0)

rglwidget()


aspect3d(1,1,1)
# Compute the linear regression (y = ax + bz + d)
y = dataSet$Poverty
x = dataSet$Income
z = dataSet$Population
#spheres3d(x, y, z, r = 0.2, color = "#D95F02") 
fit <- lm(y ~ x + z)
# predict values on regular xz grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)
# Add regression surface

rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
            alpha = 0.5, lit = FALSE)  
# Add grid lines
rgl.surface(x.pred, z.pred, y.pred, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")

with(dataSet,plot3d( 
  x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],], 
  col = data$color, 
  type = 's', 
  size = .2,
  radius = .2,
  xlab=paste(d[1]), ylab=paste(d[2]), zlab=paste(d[3]))
)
with(dataSet,text3d(x=dataSet[,d[1],], y=dataSet[,d[2],], z=dataSet[,d[3],],test))







scatter3d(Population,Income,Unemployed)
rglwidget()



library(plotly)
library(reshape2)

#load data

test2 <- cbind(dataSet[,c("Income","Population","Unemployed")],set.final$Groups,test)

#data_lm <- lm(Poverty ~ Income + Population,data = test2)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 1000

#Setup Axis
axis_x <- seq(min(test2$Income), max(test2$Income), by = graph_reso)
axis_y <- seq(min(test2$Population), max(test2$Population), by = graph_reso)
axis_z <- seq(min(test2$Unemployed), max(test2$Unemployed), by = graph_reso)

#Sample points
#data_lm_surface <- expand.grid(Income = axis_x,Population = axis_y,KEEP.OUT.ATTRS = F)
#data_lm_surface$Poverty <- predict.lm(data_lm, newdata = data_lm_surface)
#data_lm_surface <- acast(data_lm_surface, Population ~ Income, value.var = "Poverty") #y ~ x

hcolors=c("green","blue","red")[test2$set.final$Groups]
data_plot <- plot_ly(test2, 
                     x = ~Income, 
                     y = ~Population, 
                     z = ~Unemployed,
                     text = test2$test, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors))

data_plot <- add_text(test2, 
                      x = ~Income, 
                      y = ~Population, 
                      z = ~Unemployed,
                      text = test2$test, # EDIT: ~ added
                      type = "scatter"
                      #mode = "markers"
)
#data_plot <- add_trace(p = data_plot,
#                       z = data_lm_surface,
#                       x = axis_x,
#                       y = axis_y,
#                       type = "surface")

data_plot

