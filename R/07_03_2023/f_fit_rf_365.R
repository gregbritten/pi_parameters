fit_rf_365 <- function(parm){
  fits <- list()
  for(j in 1:length(regions)){
    sstj       <- sst[,region==regions[j]      & !is.na(region)]
    parj       <- par[,region==regions[j]      & !is.na(region)]
    chlj       <- chl[,region==regions[j]      & !is.na(region)]
    monthj     <- month[region==regions[j]     & !is.na(region)]
    daylengthj <- daylength[region==regions[j] & !is.na(region)]
    latj       <- lat[region==regions[j]       & !is.na(region)]
    lonj       <- lon[region==regions[j]       & !is.na(region)]
    Ekj        <- Ek[region==regions[j]        & !is.na(region)]
    alphaj     <- alpha[region==regions[j]     & !is.na(region)]
    PBmaxj     <- PBmax[region==regions[j]     & !is.na(region)]
  
    fits[[j]] <- lapply(1:90,function(i){
        
        if(i==1){X <- data.frame(PAR=parj[1:i,], SST=sstj[1:i,], CHL=chlj[1:i,],
                                 daylength=daylengthj, month=monthj, lat=latj, lon=lonj)
        }else{   X <- data.frame(PAR=t(parj[1:i,]), SST=t(sstj[1:i,]), CHL=t(chlj[1:i,]),
                                 daylength=daylengthj, month=monthj, lat=latj, lon=lonj)
        }
        
        if(parm=='Ek')    fit <- randomForest(Ekj   ~ ., data=X)
        if(parm=='alpha') fit <- randomForest(alpha ~ ., data=X)
        if(parm=='PBmax') fit <- randomForest(PBmax ~ ., data=X)
        
        list(fit, data.frame(Ek=Ekj,alpha=alphaj,PBmax=PBmaxj,X))
    })
  }
  return(fits)
}









