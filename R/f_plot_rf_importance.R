f_plot_rf_importance <- function(fits,chl=FALSE,terms,labs=FALSE,xaxt=FALSE,ntime){
  #par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
  for(j in 1:6){
    
    plot(-999,xlim=c(-ntime,1),ylim=c(0,ylims[j]),xaxt='n')
    if(labs==TRUE) mtext(nms[j],adj=0,cex=0.7)
    if(xaxt==TRUE) axis(side=1)
    if(chl==FALSE){
      for(p in 1:length(terms)){
          xx <- unlist(lapply(1:ntime,function(i){
            imp <- fits[[j]][[i]][[1]]$importance[,1]
            #imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
            imp <- abs(imp)/sum(abs(imp))
            #imp <- abs(imp)
            abs(imp[names(imp)==terms[p]])
          }))
          lines(xx,col=cols[p])
          abline(h=mean(xx,na.rm=TRUE),col=cols[p],lty=2)}}
    if(chl==TRUE){
      for(p in 1:length(terms)){
        xx <- unlist(lapply(1:ntime,function(i){
          imp <- fits[[j]][[i]][[1]]$importance[,1]
          #imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
          imp <- abs(imp)/sum(abs(imp))
          #imp <- abs(imp)
          abs(imp[names(imp)==terms[p]])
        }))
        lines(-ntime:-2,xx[ntime:2],col=cols[p])
        abline(h=mean(xx,na.rm=TRUE),col=cols[p],lty=2)}}
  }
}

