fit_rf <- function(D,parm,ntime,regions,depth){
  fits <- list()
  for(j in 1:length(regions)){
    fits[[j]] <- lapply(1:ntime,function(i){
      
      if(depth=='all'){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,depth))
        if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par + depth, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ sst + par + depth, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par + depth, data=d, importance=TRUE) 
      }
      
      if(depth=='40'){
        d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,depth),depth<=40)
        if(parm=='Ek')    fit <- randomForest(Ek    ~ sst + par + depth, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- randomForest(alpha ~ sst + par + depth, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- randomForest(PBmax ~ sst + par + depth, data=d, importance=TRUE) 
      }
      list(fit,d)
    })
  }
  # fits[[length(regions)+1]] <- lapply(1:ntime,function(i){
  #   if(full==TRUE){
  #     d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,nano_pico,micro_nano,lat,lon,depth))
  #     if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE) 
  #     if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE)
  #     if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + lat + lon + depth, data=d, importance=TRUE)    
  #   }
  # 
  #   if(full==FALSE){
  #     d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,nano_pico,micro_nano,depth))
  #     if(parm=='Ek')    fit <- randomForest(Ek    ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE) 
  #     if(parm=='alpha') fit <- randomForest(alpha ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE)
  #     if(parm=='PBmax') fit <- randomForest(PBmax ~ chl + sst + par + nano_pico + micro_nano + depth, data=d, importance=TRUE)    
  #   }
  #   list(fit,d)
  # })
  return(fits)
}
