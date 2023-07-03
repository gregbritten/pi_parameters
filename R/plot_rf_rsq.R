plot_rf_rsq <- function(fits,parm,max=TRUE,ylim_rmse,ylim_r2,legend=FALSE,text,outer=FALSE,ntime){
  #par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(3,3,2,2))
  plot(-999,xlim=c(-ntime,0),ylim=c(ylim_r2[1],ylim_r2[2]))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
    xx <- unlist(lapply(1:ntime,function(i){
      if(parm=='Ek')    imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$Ek)^2
      if(parm=='alpha') imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$alpha)^2
      if(parm=='PBmax') imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$PBmax)^2
      imp
    }))
    lines(-(ntime-1):0,xx[ntime:1],col=cols[j])
    if(max==TRUE) abline(v={-(ntime-1):0}[xx[ntime:1]==max(xx[ntime:1])],lty=3,col=cols[j])
    #abline(h=xx[30],lty=2,col=cols[j])
  }
  mtext(side=2,expression('R'^2),line=2)
  mtext(adj=0,text)  
  plot(-999,xlim=c(-ntime,0),ylim=c(ylim_rmse[1],ylim_rmse[2]))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
      xx <- unlist(lapply(1:ntime,function(i){
      if(parm=='Ek')    imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$Ek)^2))
      if(parm=='alpha') imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$alpha)^2))
      if(parm=='PBmax') imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$PBmax)^2))
      imp
    }))
    lines(-(ntime-1):0,xx[ntime:1],col=cols[j])
    if(max==TRUE) abline(v={-(ntime-1):0}[xx[ntime:1]==min(xx[ntime:1])],lty=3,col=cols[j])
  }
  #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
  mtext(side=2,expression('RMSE'),line=2)    
  if(legend==TRUE) legend('topright',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=0.8)
  if(outer==TRUE) mtext(outer=TRUE,side=1,expression('Initial Time '*italic('t'['i'])),line=1)    
}
