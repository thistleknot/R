library(plotly)
library(reshape2)

#load data
#test2 <- cbind(dataSet[,paste(d)],cl$cluster,test,set.final$Groups)
test2 <- cbind(dataSet[,paste(d)],set.final$Groups,test)
test2 <- mydata
#paste(d)
#test2 <- cbind(dataSet[,c("Income","Population","Unemployed")],set.final$Groups,test)
#d[1]
#data_lm <- lm(Poverty ~ Income + Population,data = test2)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 10

f <- as.formula(paste(paste(d[1]), paste (c(paste(d[2]),paste(d[3])), collapse=" + "), sep=" ~ ")) 

lm_model <- lm(f,data = test2)


#can't figure out how to pass dynamic names to x,y,z

hcolors=c("green","blue","red","black")[test2$as.numeric.set.final.Groups.]
hsymbols=c('circle', 'circle-open', 'square', 'square-open','diamond', 'diamond-open', 'cross', 'x')[test2$fit.cluster]

#hcolors=c("green","blue","red","black")[test2$`set.final$Groups`]
data_plot <- plot_ly(test2[,1:3],
                     y = ~test2[,1],
                     x = ~test2[,2],
                     z = ~test2[,3],
                     text = test2$test, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = test2$Poverty, symbol=hsymbols))

data_plot

# fit model


# draw 3D scatterplot
#p <- plot_ly(data = test2[,1:3], z = ~test2[,3], x = ~test2[,1], y = ~test2[,2], opacity = 0.6) %>% add_markers() 

# draw a plane
#p %>% add_surface(x = ~test2[,1], y = ~test2[,2], z = NULL, showscale = FALSE) 

if(FALSE)
{
  #Setup Axis
  axis_y <- seq(min(test2[,paste(d[1]),drop=FALSE]), max(test2[,paste(d[1]),drop=FALSE]), by = (max(test2[,paste(d[1]),drop=FALSE])-min(test2[,paste(d[1]),drop=FALSE]))/graph_reso)
  if (length(d) > 1)
  {
    axis_x <- seq(min(test2[,paste(d[2]),drop=FALSE]), max(test2[,paste(d[2]),drop=FALSE]), by = (max(test2[,paste(d[2]),drop=FALSE])-min(test2[,paste(d[2]),drop=FALSE]))/graph_reso)
  } else
    (
      axis_x <- NA
    )
  
  if (length(d)==3)
  {
    axis_z <- seq(min(test2[,paste(d[3]),drop=FALSE]), max(test2[,paste(d[3]),drop=FALSE]), by = (max(test2[,paste(d[3]),drop=FALSE])-min(test2[,paste(d[3]),drop=FALSE]))/graph_reso)  
  } else
    (
      axis_z <- NA
    )
  
  lm_surface <- expand.grid(Income = axis_y,Population = axis_z, 
                            White = axis_x, 
                            KEEP.OUT.ATTRS = F
                            #, by = graph_reso
                            )
  lm_surface$Income <- predict.lm(lm_model, newdata = lm_surface)
  lm_surface <- acast(lm_surface, Income ~ Population + White, value.var = "Income") #y ~ x
  
  
  data_plot <- add_trace(lm_surface,  y = ~test2$Income, x = ~test2$Population, z = ~test2$White, mode = "surface")
  
  data_plot <- add_trace(p = data_plot,
                         z = lm_surface,
                         x = axis_x,
                         y = axis_y,
                         type = "surface")
  
  data_plot
  
}
