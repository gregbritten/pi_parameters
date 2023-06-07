source('r/process_csvs.r') ##--data processing--###########

regions <- c('scot','lab','spac','tas','ice')
vars <- c('par','sst','chl','kd_490')
ylims <- list(c(-40,40),c(-5,5),c(-3,3),c(-0.2,0.2))
var_long <- c(expression(Delta*'PAR'),expression(Delta*'SST'),expression(Delta*'Chl'),expression(Delta*'Kd'['490']))

k <- 1
pdf('plots/histories.pdf',height=5.5,width=8)
par(mfrow=c(4,5),mar=c(0,0,0,0),oma=c(4,4,3,3),cex.axis=0.8)
for(p in 1:length(vars)){
  for(j in 1:length(regions)){
    d     <- D[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    E     <- array(NA,dim=c(dim(d)[1],dim(d)[2],30))
    for(i in 1:30){
      E[,,i] <- data.matrix(D[[i]] %>% 
                              filter(region==regions[j]))
    }
    colnames(E) <- colnames(d)
    matplot(t(E[,colnames(d)==vars[p],] - E[,colnames(d)==vars[p],1]),type='l',col=adjustcolor('black',alpha.f=0.7),lty=1,lwd=0.4,
            ylim=ylims[[p]],xaxt='n',yaxt='n')
    if(p==1) mtext(region_long[j])
    if(j==1){mtext(side=2,line=2.5,var_long[p])
             axis(side=2)}
    if(j==1 & p==4) axis(side=1)
    if(j>1 & p==4) axis(side=1,at=c(5,10,15,20,25,30))
    mtext(adj=0.025,paste0(letters[k],')'),line=-1.25)
    k <- k+1
  }
}
mtext(outer=TRUE,side=1,line=2.5,expression('Averaging Timescale [days]'))
dev.off()



