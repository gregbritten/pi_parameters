plot_histories <- function(D,vars,ntime){
  k <- 1
  for(p in 1:length(vars)){
    for(j in 1:length(regions)){
      d     <- D[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
      E     <- array(NA,dim=c(dim(d)[1],dim(d)[2],ntime))
      for(i in 1:ntime){
        E[,,i] <- data.matrix(D[[i]] %>% 
                                filter(region==regions[j]))
      }
      colnames(E) <- colnames(d)
      #plot time series after subtracting first entry
      matplot(-ntime:-1,t(E[,colnames(d)==vars[p],ntime:1] - E[,colnames(d)==vars[p],1]),type='l',col=adjustcolor('black',alpha.f=0.7),lty=1,lwd=0.4,
              ylim=ylims[[p]],xaxt='n',yaxt='n',xlim=c(-ntime,1),bty='n')
      if(p==1) mtext(region_long[j],adj=0)
      if(j==1){mtext(side=2,line=2.5,var_long[p])
        axis(side=2)}
      if(j==1 & p==3) axis(side=1)
      if(j>1 & p==3) axis(side=1)
      mtext(adj=0.025,paste0(letters[k],')'),line=-1.25)
      k <- k+1
    }
  }
  mtext(outer=TRUE,side=1,line=3,expression('Initial Time '*italic(t['i'])))
}
