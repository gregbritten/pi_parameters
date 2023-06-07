fit_rf <- function(parm,chl,full){
  fits <- list()
  for(j in 1:length(regions)){
    fits[[j]] <- lapply(1:30,function(i){
      
      if(full==TRUE){
      if(chl==FALSE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      }
      if(chl==TRUE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,lat,lon,depth,month,daylength))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      }
      }
      
      if(full==FALSE){
      if(chl==FALSE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ sst + par, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par, data=d, importance=TRUE) 
      }
      if(chl==TRUE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,lat,lon,depth,month,daylength))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par, data=d, importance=TRUE) 
      }
      }
      
      list(fit,d)
    })
  }
  fits[[length(regions)+1]] <- lapply(1:30,function(i){
    if(full==TRUE){
    if(chl==FALSE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(sst,par,lat,lon,depth,month,daylength))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
    }  
    if(chl==TRUE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,lat,lon,depth,month,daylength))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)
      if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)    
    }
    }
    
    if(full==FALSE){
    if(chl==FALSE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(sst,par,lat,lon,depth,month,daylength))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
    }  
    if(chl==TRUE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,lat,lon,depth,month,daylength))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)
      if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)    
    }
    }
    list(fit,d)
  })
  return(fits)
}
