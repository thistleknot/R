raw <- read.csv("https://raw.githubusercontent.com/thistleknot/Python-Stock/master/data/combined_set.csv",row.names=1,header=TRUE)

nv_diff_sets <- function(s,d,y,season=4,nv=NA)
{
  '
  d=1
  s=1
  y=raw[,"CSUSHPINSA",drop=FALSE]
  nv=c(6,7)
  '
  temp <- y
  
  set <- temp
  
  if(s==1)
  {
    
    #apply seasonal difference
    #temp <- as.data.frame(unlist(lapply((temp-dplyr::lag(temp,1*season)),function(x){ifelse(is.nan(x),return(0),return(x))})))
    temp <- temp-dplyr::lag(temp,1*season)
    
    colnames(temp) <- c("s")
    
    set <- cbind(set, temp)
  }
   
  for (d in 1:d)
  {#d=1
    
    temp <- temp-dplyr::lag(temp,1)
    colnames(temp) <- d
    set <- cbind(set, temp)
    
  }
  
  rownames(set) <- rownames(y)
  #extended <- c(temp_4,tail(c(c(temp_3),tail(temp_4,1)+cumsum(tail(temp_3,1)+cumsum(nv))),length(nv)))
 
  if (d==1&s==1)
  {
    temp3 <- dplyr::lag(set[,"s"],1)+set[,"1"]
    newSet <- dplyr::lag(set[,colnames(y)] ,season) + temp3
    
    ns_nt3 <- tail(set[,"s"],1)+cumsum(nv)
    extended <- head(tail(set[,colnames(y)],season),length(ns_nt3))+ns_nt3
    
  }else if (d==2&s==0) 
    {
    temp3 <- dplyr::lag(set[,"1"],1)+set[,"2"]
    #newSet <- (dplyr::lag(set[,1] ,1) + temp3)
    
    temp3[is.na(temp3)]=0
    cs <- cumsum(temp3)
    newSet <- set[1,1] + cs
    set <- cbind(set,temp3,cs,newSet)
    
    extended <- tail(set[,"cs"],1)+set[1,1]+cumsum(nv)
    
  } else if (d==1&s==0)
  {
    temp3 <- set[,"1"]
    
    temp3[is.na(temp3)]=0
    cs <- cumsum(temp3)
    newSet <- set[1,1] + cs
    set <- cbind(set,temp3,cs,newSet)
    
    extended <- tail(set[,"cs"],1)+set[1,1]+cumsum(nv)
    
    #newSet <- (dplyr::lag(set[,1] ,1) + temp3)
    
  }
  
  
  return(extended)
  
}

nv_diff_sets(1,1,raw[,"CSUSHPINSA",drop=FALSE],4,c(1,2))

