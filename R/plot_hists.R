plot_hists <- function(D){
hist_at <- seq(-1.1,1.1,0.06)
  for(j in 1:length(regions)){
    d     <- D[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    E     <- array(NA,dim=c(dim(d)[1],dim(d)[2],30))
    for(i in 1:30) E[,,i] <- data.matrix(D[[i]] %>% filter(region==regions[j]))
    colnames(E) <- colnames(d)
    
    dd <- t(E[,colnames(d)==vars[1],30:1]) %>% .[,apply(.,2,function(x) sum(is.finite(x))>5)]
    cors <- apply(dd,2,function(x){
      n <- length(x)
      return(cor(x[2:n],x[1:(n-1)],use='complete.obs'))
    })
    
    hist(cors,main='',breaks=hist_at,xlim=c(0,1),freq=FALSE,ylim=c(0,17),col=cols[1])
    mtext(nms[j],adj=0)
    abline(v=mean(cors,na.rm=TRUE),lty=2,col=cols[1])
    if(j==1) legend('topleft',bty='n',legend=c('Chl','SST','PAR'),col=cols[c(3,2,1)],pch=15,cex=1.5)
    
    for(p in 3:2){
      dd <- t(E[,colnames(d)==vars[p],30:1]) %>% .[,apply(.,2,function(x) sum(is.finite(x))>5)]
      cors <- apply(dd,2,function(x){
        n <- length(x)
        return(cor(x[2:n],x[1:(n-1)],use='complete.obs'))
      })
      hist(cors,breaks=hist_at,add=TRUE,col=adjustcolor(cols[p],alpha.f=0.7),freq=FALSE)
      abline(v=mean(cors,na.rm=TRUE),col=cols[p],lty=2)
    }
  }
  mtext(outer=TRUE,side=1,'Autocorrelation Coefficient',line=2.5)
  mtext(outer=TRUE,side=2,'Frequency Density',line=0.5)
}
