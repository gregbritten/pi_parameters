library(mgcv)
library(randomForest)
library(doParallel)
ncores <- detectCores()      
registerDoParallel(ncores-1) #leave one core for other tasks

source('r/process_csvs.r') ##--data processing--###########

regions <- c('scot','lab','spac','tas','ice')

fits=fits_chl <- list()

for(j in 1:length(regions)){
  fits[[j]] <- lapply(1:30,function(i){
    d     <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength))
    randomForest(Ek ~ sst + par + lat + lon + depth + month + daylength, 
                 data=d, importance=TRUE)
  })
  fits_chl[[j]] <- lapply(1:30,function(i){
    d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    randomForest(Ek ~ chl + kd_490 + sst + par + lat + lon + depth + month + daylength, 
                 data=d, importance=TRUE)
  })
}

cols <- brewer.pal(7,'Dark2')
lets <- c('a)','b)','c)','d)','e)')
nms <- numeric(5)
ylims=c(1200,2000,4000,1000,500)
for(i in 1:5) nms[i] <- paste(lets[i],region_long[i])


plot_rfs <- function(chl=FALSE){
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
for(j in 1:5){
  plot(-999,xlim=c(0,30),ylim=c(0,ylims[j]))
  mtext(nms[j],adj=0)  
  for(p in 1:7){
    lines(unlist(lapply(1:30,function(i){
    if(chl==FALSE) imp <- fits[[j]][[i]]$importance[,1]
    if(chl==TRUE) imp <- fits_chl[[j]][[i]]$importance[,1]
    imp[p]
})),col=cols[p])
  }
  #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
  if(j==1) legend('topleft',legend=c('sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
}
mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
mtext(outer=TRUE,side=2,'Relative Variable Importance [normalized %MSE increase]',line=1)    
}

plot_rfs(chl=FALSE)
plot_rfs(chl=TRUE)

##--DO GLOBAL--##############################
d_global <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength))

