plot_rf_importance <- function(fits,chl=FALSE,terms,labs=FALSE,xaxt=FALSE){
  #par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
  for(j in 1:5){
    
    plot(-999,xlim=c(-30,0),ylim=c(0,ylims[j]),xaxt='n')
    if(labs==TRUE) mtext(nms[j],adj=0)
    if(xaxt==TRUE) axis(side=1)
    if(chl==FALSE){
      for(p in 1:length(terms)){
          xx <- unlist(lapply(1:30,function(i){
            imp <- fits[[j]][[i]][[1]]$importance[,1]
            imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
            #imp <- abs(imp)
            abs(imp[names(imp)==terms[p]])
          }))
          lines(xx,col=cols[p])
          abline(h=mean(xx),col=cols[p],lty=2)}}
    if(chl==TRUE){
      for(p in 1:length(terms)){
        xx <- unlist(lapply(1:30,function(i){
          imp <- fits[[j]][[i]][[1]]$importance[,1]
          imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
          #imp <- abs(imp)
          abs(imp[names(imp)==terms[p]])
        }))
        lines(-30:-1,xx[30:1],col=cols[p])
        abline(h=mean(xx),col=cols[p],lty=2)}}
  }
}

