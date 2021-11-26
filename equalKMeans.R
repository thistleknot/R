library(magrittr); library(dplyr); library(ggplot2)

#https://rviews.rstudio.com/2019/06/13/equal-size-kmeans/

data(mtcars)
k = 3
kdat = mtcars %>% select(c(mpg, wt))
kdat %>% kmeans(k) -> kclust

kdist = function(x1, y1, x2, y2){
  sqrt((x1-x2)^2 + (y1-y2)^2)
}

kMeanAdj = function(NewCenters, kdat, iter = 3, k)
  {
  
  
  centers = kclust$centers
  
  kdat %<>% 
    mutate(D1 = kdist(mpg, wt, centers[1,1], centers[1,2]))
  kdat %<>% 
    mutate(D2 = kdist(mpg, wt, centers[2,1], centers[2,2]))
  kdat %<>% 
    mutate(D3 = kdist(mpg, wt, centers[3,1], centers[3,2]))
  
  kdat$assigned = 0
  kdat$index = 1:nrow(kdat)
  working = kdat
  FirstRound = nrow(kdat) - (nrow(kdat) %% k)
  
  for(i in 1:FirstRound){ 
    #cluster counts can be off by 1 due to uneven multiples of k. 
    j = if(i %% k == 0) k else (i %% k)
    itemloc = 
      working$index[which(working[,(paste0("D", j))] ==
                            min(working[,(paste0("D",j))]))[1]]
    kdat$assigned[kdat$index == itemloc] = j
    working %<>% filter(!index == itemloc)
    ##The sorting hat says... GRYFFINDOR!!! 
  }
  for(i in 1:nrow(working)){
    #these leftover points get assigned to whoever's closest, without regard to k
    kdat$assigned[kdat$index ==
                    working$index[i]] = 
      which(working[i,3:5] == min(working[i, 3:5])) 
  }
  
  NewCenters <- kdat %>% filter(assigned == 1) %>% 
    select(mpg, wt) %>%
    kmeans(1) %$% centers
  
  NewCenters %<>% rbind(kdat %>% 
                          filter(assigned == 2) %>%
                          select(mpg, wt) %>%
                          kmeans(1) %$% centers)
  
  NewCenters %<>% rbind(kdat %>%
                          filter(assigned == 3) %>%
                          select(mpg, wt) %>%
                          kmeans(1) %$% centers)
  
  NewCenters %<>% as.data.frame()
  
  kdat$assigned %<>% as.factor()
  kdat %>% ggplot(aes(x = mpg, y = wt, color = assigned)) +
    theme_minimal() + geom_point() + 
    geom_point(data = NewCenters, aes(x = mpg, y = wt),
               color = "black", size = 4) + 
    geom_point(data = as.data.frame(centers), 
               aes(x = mpg, y = wt), color = "grey", size = 4)
  
  return(NewCenters)
}

x = kMeanAdj(NewCenters, kdat, iter = 3, k) 

x$Data$assigned %<>% as.factor()

x$Data %>% ggplot(aes(x = mpg, y = wt, color = assigned)) +
  theme_minimal() +  geom_point() +
  geom_point(data = x$centers, aes(x = mpg, y=wt),
             color = "black", size = 4)


