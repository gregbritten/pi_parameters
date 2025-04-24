library(maps)
library(viridis)
library(here)

FITS <- readRDS('results/FITS.rds')

sst <- readRDS('processed_data/sst.rds')
par <- readRDS('processed_data/par.rds')
chl <- readRDS('processed_data/chl.rds')

##--construct climatologies using timescale that maximizes predictability--#######
day_Ek    <- 34
day_PBmax <- 24

##--34 days--##########
ii_34 <- c((365-day_Ek):365,1:365)
chl_avg_34=par_avg_34=sst_avg_34 <- array(NA,dim=c(360,180,365))
for(i in (day_Ek+1):(365+day_Ek)){
  print(i)
  chl_avg_34[,,i-day_Ek] <- apply(chl[,,ii_34[(i-(day_Ek+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
  par_avg_34[,,i-day_Ek] <- apply(par[,,ii_34[(i-(day_Ek+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
  sst_avg_34[,,i-day_Ek] <- apply(sst[,,ii_34[(i-(day_Ek+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
}

##--24 days--############
ii_24 <- c((365-day_PBmax):365,1:365)
chl_avg_24=par_avg_24=sst_avg_24 <- array(NA,dim=c(360,180,365))
for(i in (day_PBmax +1):(365+day_PBmax)){
  print(i)
  chl_avg_24[,,i-day_PBmax] <- apply(chl[,,ii_24[(i-(day_PBmax+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
  par_avg_24[,,i-day_PBmax] <- apply(par[,,ii_24[(i-(day_PBmax+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
  sst_avg_24[,,i-day_PBmax] <- apply(sst[,,ii_24[(i-(day_PBmax+1)):(i+1)]],MARGIN=c(1,2),function(x) mean(x,na.rm=TRUE))  
}

##--make predictions-######################
regs <- c("scot","lab","ice","spac","tas")

pred_f <- function(chl,sst,par,day,fit){
  XX <- data.frame(chl=c(chl[,,day]),sst=c(sst[,,day]),par=c(par[,,day]))
  ii <- complete.cases(XX)
  XX$pico[ii]  <- pico(G,H,J,K,C=XX$chl[ii],SST=XX$sst[ii])
  XX$depth     <- 1 #predictions at surface
  
  pred <- array(NA,dim=c(360,180,5))
  for(i in 1:5){
    print(i)
    XX$region <- regs[i]
    pred[,,i] <- predict(fit,newdata=XX) %>%
      matrix(.,nrow=360,ncol=180,byrow=FALSE)
  }
  return(apply(pred,c(1,2),function(x) mean(x,na.rm=TRUE)))
}
  

##--make maps--##############
pdf('plots/maps.pdf',height=5,width=10)
par(mfrow=c(2,3),mar=c(1,1,1,1),oma=c(2,2,2,4),cex.lab=0.8,oma=c(3,3,2,2))

##--PBmax--#######
zlims <- c(2,6)
ncols <- 15
cols  <- turbo(ncols)
image(lons,lats,pred_f(chl=chl_avg_24,sst=sst_avg_24,par=par_avg_24,
                            day=1,fit=FITS[["reg"]][["PBmax"]][[day_PBmax]]),
           xaxt='n',col=cols,zlim=zlims)  
  map(add=TRUE,col='grey',fill=TRUE,border=NA)
  axis(side=1,labels=NA)
  mtext(expression('a)'~italic('P'['B']^{'max'})),adj=0)
  mtext('January 1',adj=1)
  
image(lons,lats,pred_f(chl=chl_avg_24,sst=sst_avg_24,par=par_avg_24,
                       day=187,fit=FITS[["reg"]][["PBmax"]][[day_PBmax]]),
           xaxt='n',yaxt='n',col=cols,zlim=zlims)  
  map(add=TRUE,col='grey',fill=TRUE,border=NA)
  axis(side=1,labels=NA);axis(side=2,labels=NA)
  mtext(expression('b)'~italic('P'['B']^{'max'})),adj=0)
  mtext('July 1',adj=1)
plot.new()
image.plot(matrix(zlims),col=cols,legend.only=TRUE)

##--Ek--####
zlims <- c(0,350)
ncols <- 8*3
cols <- turbo(ncols)
image(lons,lats,pred_f(chl=chl_avg_34,sst=sst_avg_34,par=par_avg_34,
                  day=1,fit=FITS[["reg"]][["Ek"]][[day_Ek]]),
      col=cols,zlim=zlims)  
  map(add=TRUE,col='grey',fill=TRUE,border=NA)
  mtext(expression('c)'~italic('E'['k'])),adj=0)
  mtext('January 1',adj=1)
image(lons,lats,pred_f(chl=chl_avg_34,sst=sst_avg_34,par=par_avg_34,
                  day=187,fit=FITS[["reg"]][["Ek"]][[day_Ek]]),
      col=cols,zlim=zlims,yaxt='n')  
  map(add=TRUE,col='grey',fill=TRUE,border=NA)
  mtext(expression('d)'~italic('E'['k'])),adj=0)
  mtext('July 1',adj=1)
plot.new()
image.plot(matrix(zlims),col=cols,legend.only=TRUE)

dev.off()
