#source('r/process_csvs.r') ##--data processing--###########

regions <- c('scot','lab','spac','tas','ice')
vars <- c('par','sst','chl')
ylims <- list(c(-40,40),c(-5,5),c(-3,3))
var_long <- c(expression(Delta*bar('PAR')*'('*italic('t'['i'])*')'),expression(Delta*'SST'),expression(Delta*'Chl'))

pdf('plots/histories.pdf',height=5,width=8)
k <- 1
par(mfrow=c(3,5),mar=c(0,0,0,0),oma=c(4,5,3,3),cex.axis=0.8)
for(p in 1:length(vars)){
  for(j in 1:length(regions)){
    d     <- D[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    E     <- array(NA,dim=c(dim(d)[1],dim(d)[2],30))
    for(i in 1:30){
      E[,,i] <- data.matrix(D[[i]] %>% 
                              filter(region==regions[j]))
    }
    colnames(E) <- colnames(d)
    #plot time series after subtracting first entry
    matplot(-30:-1,t(E[,colnames(d)==vars[p],30:1] - E[,colnames(d)==vars[p],1]),type='l',col=adjustcolor('black',alpha.f=0.7),lty=1,lwd=0.4,
            ylim=ylims[[p]],xaxt='n',yaxt='n',xlim=c(-30,1))
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
dev.off()

###################################################################
##--RAW TIME SERIES--##############################################
###################################################################
regions <- c('scot','lab','spac','tas','ice')
vars <- c('sat_par','sst','chl')
ylims <- list(c(-40,40),c(-5,5),c(-3,3))
var_long <- c(expression(Delta*bar('PAR')*'('*italic('t'['i'])*')'),expression(Delta*'SST'),expression(Delta*'Chl'))

k <- 1
par(mfrow=c(3,5),mar=c(0,0,0,0),oma=c(4,5,3,3),cex.axis=0.8)
for(p in 1:length(vars)){
  for(j in 1:length(regions)){
    d     <- L[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
    E     <- array(NA,dim=c(dim(d)[1],dim(d)[2],30))
    for(i in 1:30){
      E[,,i] <- data.matrix(L[[i]] %>% 
                              filter(region==regions[j]))
    }
    colnames(E) <- colnames(d)
    #plot time series after subtracting first entry
    matplot(-30:-1,t(E[,colnames(d)==vars[p],30:1] - E[,colnames(d)==vars[p],1]),type='l',col=adjustcolor('black',alpha.f=0.7),lty=1,lwd=0.4,
            ylim=ylims[[p]],xaxt='n',yaxt='n',xlim=c(-30,1))
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


