plot_rf_rsq <- function(fits,chl,parm,ylim_rmse,ylim_r2,legend=FALSE,text){
  #par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(3,3,2,2))
  plot(-999,xlim=c(0,30),ylim=c(ylim_r2[1],ylim_r2[2]))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
    lines(unlist(lapply(1:30,function(i){
      if(parm=='Ek')    imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$Ek)^2
      if(parm=='alpha') imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$alpha)^2
      if(parm=='PBmax') imp <- cor(predict(fits[[j]][[i]][[1]]),fits[[j]][[i]][[2]]$PBmax)^2
      imp
    })),col=cols[j])
  }
  mtext(side=2,expression('R'^2),line=2.5)
  mtext(adj=0,text)  
  plot(-999,xlim=c(0,30),ylim=c(ylim_rmse[1],ylim_rmse[2]))
  for(j in 1:5){
    #mtext(nms[j],adj=0)
    lines(unlist(lapply(1:30,function(i){
      if(parm=='Ek') imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$Ek)^2))
      if(parm=='alpha') imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$alpha)^2))
      if(parm=='PBmax') imp <- sqrt(mean((predict(fits[[j]][[i]][[1]])-fits[[j]][[i]][[2]]$PBmax)^2))
      imp
    })),col=cols[j])
  }
  #plot(-999,xlim=c(0,30),ylim=c(-0.05,0.6))
  mtext(side=2,expression('RMSE'),line=2.5)    
  if(legend==TRUE) legend('topright',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=0.8)
  mtext(outer=TRUE,side=1,'Averaging Timescale [days]',line=1)    
}
