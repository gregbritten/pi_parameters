plot_rf_importance_cor <- function(fits,chl=FALSE,terms,labs=FALSE,xaxt=FALSE){
  #par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
  library(corrplot)
  library(RColorBrewer)
  scalebluered <- colorRampPalette(brewer.pal(8, "RdBu"))(8)[8:1]
  
  for(j in 1:5){
    #plot(-999,xlim=c(0,30),ylim=c(0,ylims[j]),xaxt='n')
    if(xaxt==TRUE) axis(side=1)
    if(chl==FALSE){
      xx <- matrix(NA,nrow=30,ncol=length(terms))
      for(p in 1:length(terms)){
        xx[,p] <- unlist(lapply(1:30,function(i){
          imp <- fits[[j]][[i]][[1]]$importance[,1]
          #imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
          imp <- abs(imp)
          abs(imp[names(imp)==terms[p]])
        }))
        corrplot(cor(xx),type='upper')}}
    if(chl==TRUE){
      xx <- matrix(NA,nrow=30,ncol=length(terms))
      for(p in 1:length(terms)){
        xx[,p] <- unlist(lapply(1:30,function(i){
          imp <- fits[[j]][[i]][[1]]$importance[,1]
          #imp <- abs(imp)/sum(abs(imp[names(imp)%in%terms]))
          imp <- abs(imp)
          abs(imp[names(imp)==terms[p]])
        }))
      }
      colnames(xx) <- c('Chl','SST','PAR')
      corrplot(cor(xx), method="shade", shade.col=NA, tl.col="black", 
               tl.srt=45, addgrid.col="black", type="lower", diag=FALSE, cl.pos="n")
      if(labs==TRUE) mtext(nms[j],adj=0)
      
            #corrplot(cor(xx),type='upper',diag=FALSE,mar=c(1,1,1,1))
      }
    #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
    #if(j==1 & chl==FALSE) legend('topleft',legend=c('sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
    #if(j==1 & chl==TRUE) legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), lty=1, col=cols,bty='n',cex=0.8)
  }
  #mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
  #mtext(outer=TRUE,side=2,'Variable Importance [MSE increase]',line=1)    
  plot(-999,xlim=c(0,3),ylim=c(0,3),bty='n',xaxt='n',yaxt='n')
  colorlegend(xlim=c(1,3), ylim=c(1,1.5), scalebluered, c(seq(-1,1,.5)), align="l", vertical=TRUE, addlabels=TRUE)
  
}

