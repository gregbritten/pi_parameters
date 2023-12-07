fit_rf <- function(D,parm,chl,full,ntime,regions){
  fits <- list()
  for(j in 1:length(regions)){
    fits[[j]] <- lapply(1:ntime,function(i){
      
      if(full==TRUE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,nano_pico,micro_nano,lat,lon,depth))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE) 
      }
      
      if(full==FALSE){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,nano_pico,micro_nano,depth))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE) 
      }
      
      list(fit,d)
    })
  }
  fits[[length(regions)+1]] <- lapply(1:ntime,function(i){
    if(full==TRUE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,nano_pico,micro_nano,lat,lon,depth))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE)
      if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE)    
    }

    if(full==FALSE){
      d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,nano_pico,micro_nano,depth))
      if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE) 
      if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE)
      if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE)    
    }
    list(fit,d)
  })
  return(fits)
}
