plot_lm_importance <- function(fits,chl=FALSE,terms){
  #par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
  for(j in 1:5){
    plot(NA,xlim=c(0,30),ylim=c(0,ylims[j]))
    mtext(nms[j],adj=0)
    if(chl==FALSE){
      for(p in 1:length(terms)){
        lines(unlist(lapply(1:30,function(i){
          imp <- summary(fits[[j]][[i]][[1]])$coefficients[,3]
          abs(imp[names(imp)==terms[p]])
        })),col=cols[p])}}
    if(chl==TRUE){
      for(p in 1:length(terms)){
        lines(unlist(lapply(1:30,function(i){
          imp <- summary(fits[[j]][[i]][[1]])$coefficients[,3]
          abs(imp[names(imp)==terms[p]])
        })),col=cols[p])}}
    #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
    #if(j==1 & chl==FALSE) legend('topleft',legend=c('sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
    #if(j==1 & chl==TRUE) legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
  }
  mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
  mtext(outer=TRUE,side=2,'Variable Importance [Effect size]',line=1)    
}
