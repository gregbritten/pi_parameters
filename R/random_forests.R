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
    list(randomForest(Ek ~ sst + par + lat + lon + depth + month + daylength, 
                 data=d, importance=TRUE), d)
  })
  fits_chl[[j]] <- lapply(1:30,function(i){
    d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    list(randomForest(Ek ~ chl + kd_490 + sst + par + lat + lon + depth + month + daylength, 
                 data=d, importance=TRUE), d)
  })
}




plot_rf_importance <- function(chl=FALSE){
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
for(j in 1:5){
  plot(-999,xlim=c(0,30),ylim=c(0,ylims[j]))
  mtext(nms[j],adj=0)
  if(chl==FALSE){
    for(p in 1:7){
      lines(unlist(lapply(1:30,function(i){
        imp <- fits[[j]][[i]][[1]]$importance[,1]
        imp[p]
      })),col=cols[p])}}
  if(chl==TRUE){
    for(p in 1:9){
      lines(unlist(lapply(1:30,function(i){
        imp <- fits_chl[[j]][[i]][[1]]$importance[,1]
        imp[p]
      })),col=cols[p])}}
  #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
  if(j==1 & chl==FALSE) legend('topleft',legend=c('sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
  if(j==1 & chl==TRUE) legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
}
mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
mtext(outer=TRUE,side=2,'Relative Variable Importance [normalized %MSE increase]',line=1)    
}

#Plot setup
cols <- turbo(9)
lets <- c('a)','b)','c)','d)','e)')
nms <- numeric(5)
for(i in 1:5) nms[i] <- paste(lets[i],region_long[i])
ylims=c(1200,2000,4000,1000,500)

#Make plot
plot_rf_importance(chl=FALSE)
plot_rf_importance(chl=TRUE)


###################################################################################
###################################################################################

plot_rf_rq <- function(chl=FALSE){
  par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(3,3,2,2))
  plot(-999,xlim=c(0,30),ylim=c(0,1))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
      lines(unlist(lapply(1:30,function(i){
        if(chl==FALSE) imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$Ek)^2
        if(chl==TRUE) imp <- cor(predict(fits_chl[[j]][[i]][[1]]),fits_chl[[j]][[i]][[2]]$Ek)^2
        imp
      })),col=cols[j])
  }
  mtext(side=2,expression('R'^2),line=2.5)    
  plot(-999,xlim=c(0,30),ylim=c(10,sqrt(5000)))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
    lines(unlist(lapply(1:30,function(i){
      if(chl==FALSE) imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$Ek)^2))
      if(chl==TRUE) imp <- sqrt(mean((predict(fits_chl[[j]][[i]][[1]])-fits_chl[[j]][[i]][[2]]$Ek)^2))
      imp
    })),col=cols[j])
  }
  #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
  mtext(side=2,expression('RMSE'),line=2.5)    
  legend('topright',legend=region_long, lty=1, col=cols,bty='n',cex=0.8)
  mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
}

cols <- turbo(6)
pdf('plots/rq_timescale.pdf',height=4,width=4)
plot_rf_rq(chl=FALSE)
plot_rf_rq(chl=TRUE)
dev.off()

###################################################################################
##--ADD GLOBAL TO PLTOS?
###################################################################################
d_global <- D[[i]] %>% filter(region==regions[j],complete.cases(sst,par,lat,lon,depth,month,daylength))

