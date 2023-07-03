fit_lm <- function(parm,chl,full){
  fits <- list()
  for(j in 1:length(regions)){
    fits[[j]] <- lapply(1:30,function(i){
      
      if(full==TRUE){
        if(chl==FALSE){
          d     <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength)) %>%
            mutate(sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
          if(parm=='Ek')    fit <- lm(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d) 
          if(parm=='alpha') fit <- lm(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d) 
          if(parm=='PBmax') fit <- lm(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d) 
        }
        if(chl==TRUE){
          d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,lat,lon,depth,month,daylength)) %>%
            mutate(chl=scale(chl),sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
          if(parm=='Ek')    fit <- lm(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d) 
          if(parm=='alpha') fit <- lm(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d) 
          if(parm=='PBmax') fit <- lm(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d) 
        }
      }
      
      if(full==FALSE){
        if(chl==FALSE){
          d     <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength)) %>%
            mutate(sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
          if(parm=='Ek')    fit <- lm(Ek    ~ sst + par, data=d) 
          if(parm=='alpha') fit <- lm(alpha ~ sst + par, data=d) 
          if(parm=='PBmax') fit <- lm(PBmax ~ sst + par, data=d) 
        }
        if(chl==TRUE){
          d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,sst,par,lat,lon,depth,month,daylength)) %>%
            mutate(chl=scale(chl),sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
          if(parm=='Ek')    fit <- lm(Ek    ~ chl + sst + par, data=d) 
          if(parm=='alpha') fit <- lm(alpha ~ chl + sst + par, data=d) 
          if(parm=='PBmax') fit <- lm(PBmax ~ chl + sst + par, data=d) 
        }
      }
      
      list(fit,d)
    })
  }
  fits[[length(regions)+1]] <- lapply(1:30,function(i){
    if(full==TRUE){
      if(chl==FALSE){
        d     <- D[[i]] %>% filter(region%in%regions,complete.cases(sst,par,lat,lon,depth,month,daylength)) %>%
          mutate(sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
        if(parm=='Ek')    fit <- lm(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- lm(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='PBmax') fit <- lm(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
      }  
      if(chl==TRUE){
        d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,lat,lon,depth,month,daylength)) %>%
          mutate(chl=scale(chl),sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
        if(parm=='Ek')    fit <- lm(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d) 
        if(parm=='alpha') fit <- lm(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d)
        if(parm=='PBmax') fit <- lm(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d)    
      }
    }
    
    if(full==FALSE){
      if(chl==FALSE){
        d     <- D[[i]] %>% filter(region%in%regions,complete.cases(sst,par,lat,lon,depth,month,daylength)) %>%
          mutate(sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
        if(parm=='Ek')    fit <- lm(Ek    ~ sst + par + lat + lon + depth + month + daylength, data=d) 
        if(parm=='alpha') fit <- lm(alpha ~ sst + par + lat + lon + depth + month + daylength, data=d) 
        if(parm=='PBmax') fit <- lm(PBmax ~ sst + par + lat + lon + depth + month + daylength, data=d) 
      }  
      if(chl==TRUE){
        d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,sst,par,lat,lon,depth,month,daylength))%>%
          mutate(chl=scale(chl),sst=scale(sst),par=scale(par),lat=scale(lat),lon=scale(lon),depth=scale(depth),month=scale(month),daylength=scale(daylength))
        if(parm=='Ek')    fit <- lm(Ek    ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE) 
        if(parm=='alpha') fit <- lm(alpha ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)
        if(parm=='PBmax') fit <- lm(PBmax ~ chl + sst + par + lat + lon + depth + month + daylength, data=d, importance=TRUE)    
      }
    }
    
    list(fit,d)
  })
  return(fits)
}
