
library(maps)
library(viridis)
library(here)

source('r/main_process_nc.R')
FITS <- readRDS('results/FITS.rds')

day_Ek    <- 25
day_PBmax <- 30

chl_avg_34 <- readRDS('processed_data/chl_avg_34.rds')
par_avg_34 <- readRDS('processed_data/par_avg_34.rds')
sst_avg_34 <- readRDS('processed_data/sst_avg_34.rds')

chl_avg_24 <- readRDS('processed_data/chl_avg_24.rds')
par_avg_24 <- readRDS('processed_data/par_avg_24.rds')
sst_avg_24 <- readRDS('processed_data/sst_avg_24.rds')

#Ek
regs <- c("scot","lab","ice","spac","tas")

lats <- seq(-90,90,length.out=180)
lons <- seq(-180,180,length.out=360)

dd <- D_nc[[day_Ek]] %>% filter(complete.cases(sst,par,depth,pico),depth<=mld,region==regs[[1]])


get_index <- function(lat,lon){
    lat_dist <- (lat - lats)^2
    lon_dist <- (lon - lons)^2
    i_lat <- which(lat_dist == min(lat_dist))[1]
    i_lon <- which(lon_dist == min(lon_dist))[1]
    return(c(i_lon,i_lat))
}


pdf('plots/seasonal.pdf',height=6.6,width=10)
labels <- c("a) Scotian Shelf", "f)", "k)",
            "b) Labrador Sea", "g)", "l)",
            "c) Iceland Shelf", "h)", "m)",
            "d) South Pacific", "i)", "n)",
            "e) Southern Ocean", "j)", "o)")
par(mfrow=c(length(regs),3),mar=c(0,3.5,0,1),oma=c(4,4,2,2),cex.lab=0.7,cex.axis=0.85)

k <- 1
for(j in 1:length(regs)){
    dd_Ek    <- D_nc[[day_Ek]] %>% filter(complete.cases(sst,par,depth,pico),depth<=mld,region==regs[[j]])
    dd_PBmax <- D_nc[[day_PBmax]] %>% filter(complete.cases(sst,par,depth,pico),depth<=mld,region==regs[[j]])

    plot(-999,xlim=c(0,365),ylim=c(-2.4,3.2),xaxt='n',ylab='',xlab='')
    for(i in 1:nrow(dd_Ek)){    
        ii <- get_index(dd_PBmax$lat[i],dd_PBmax$lon[i])
        XX <- data.frame(chl=c(chl_avg_34[ii[1],ii[2],]),sst=c(sst_avg_34[ii[1],ii[2],]),par=c(par_avg_34[ii[1],ii[2],]),depth=rep(1,365))
        #XX$pico  <- pico(G,H,J,K,C=XX$chl,SST=XX$sst)
        matplot(scale(XX)[,1:3],type='l',lty=1,col=adjustcolor(c('dark green','red','orange'),alpha.f=0.3),add=TRUE)
    }

    #plot(-999,xlim=c(0,365),xaxt='n',ylim=c(0,1))
    #for(i in 2:nrow(dd_Ek)){    
    #    ii <- get_index(dd_PBmax$lat[i],dd_PBmax$lon[i])
    #    chlx <- chl_avg_34[ii[1],ii[2],]
    #    lines(chlx,col=adjustcolor('dark green',alpha.f=0.3))
    #}    
    #par(new=TRUE)
    #plot(XX[,2],type='l',lty=1,col=adjustcolor('red',alpha.f=0.3),xaxt='n',ylim=c(2,20))
    #for(i in 2:nrow(dd_Ek)){    
    #    ii <- get_index(dd_PBmax$lat[i],dd_PBmax$lon[i])
    #    sstx <- sst_avg_34[ii[1],ii[2],]
    #    lines(sstx,col=adjustcolor('red',alpha.f=0.3))
    #}    
    #par(new=TRUE)
    #plot(XX[,3]*factor,type='l',lty=1,col=adjustcolor('orange',alpha.f=0.3),xaxt='n',ylim=c(0,300))
    #for(i in 2:nrow(dd_Ek)){    
    #    ii <- get_index(dd_PBmax$lat[i],dd_PBmax$lon[i])
    #    parx <- par_avg_34[ii[1],ii[2],]*factor
    #    lines(parx,col=adjustcolor('orange',alpha.f=0.3))
    #}    

    
    mtext(labels[[k]],line=-1.25,adj=0.05,cex=0.8); k=k+1  
    if(j==5) mtext("Day of Year",side=1,line=2.5)
    if(j==length(regs)) axis(side=1)
    if(j==3) mtext(side=2,line=5,c('Chl [z-score]'),col='dark green')
    if(j==3) mtext(side=2,line=3.75,c('SST [z-score]'),col='red')
    if(j==3) mtext(side=2,line=2.5,c('PAR [z-score]'),col='orange')

    plot(-999,xlim=c(0,365),ylim=c(2,5.7),xaxt='n',ylab='',xlab='')
    preds <- matrix(NA,nrow=365,ncol=nrow(dd_PBmax))
    for(i in 1:nrow(dd_PBmax)){    
        ii <- get_index(dd_PBmax$lat[i],dd_PBmax$lon[i])
        XX <- data.frame(chl=c(chl_avg_34[ii[1],ii[2],]),sst=c(sst_avg_34[ii[1],ii[2],]),par=c(par_avg_34[ii[1],ii[2],]),depth=rep(1,365))
        XX$pico  <- pico(G,H,J,K,C=XX$chl,SST=XX$sst)
        XX$region <- regs[[j]]
        pred <- predict(FITS[["reg"]][["PBmax"]][[day_PBmax]], newdata=XX)
        lines(pred,col=adjustcolor('grey',alpha.f=0.2))  
        preds[,i] <- pred   
    }
    lines(rowMeans(preds,na.rm=TRUE),lwd=2)    
    if(j==5) mtext("Day of Year",side=1,line=2.5)
    if(j==length(regs)) axis(side=1)
    if(j==3) mtext(side=2,line=2.25,expression(italic('P'['max']^'B')~'[mg C (mg chla)'^{-1}~'h'^{-1}*']'))
    mtext(labels[[k]],line=-1.25,adj=0.05,cex=0.8); k=k+1  

    plot(-999,xlim=c(0,365),ylim=c(50,300),xaxt='n',ylab='',xlab='')
    preds <- matrix(NA,nrow=365,ncol=nrow(dd_Ek))
    for(i in 1:nrow(dd_Ek)){    
        ii <- get_index(dd_Ek$lat[i],dd_Ek$lon[i])
        XX <- data.frame(chl=c(chl_avg_34[ii[1],ii[2],]),sst=c(sst_avg_34[ii[1],ii[2],]),par=c(par_avg_34[ii[1],ii[2],]),depth=rep(1,365))
        XX$pico  <- pico(G,H,J,K,C=XX$chl,SST=XX$sst)
        XX$region <- regs[[j]]
        pred <- predict(FITS[["reg"]][["Ek"]][[day_Ek]], newdata=XX) 
        lines(pred,col=adjustcolor('grey',alpha.f=0.2))
        preds[,i] <- pred 
    }    
    lines(rowMeans(preds,na.rm=TRUE),lwd=2)    
    if(j==length(regs)) axis(side=1)
    if(j==3) mtext(side=2,line=2.25,expression(italic('E'['k'])~'['*mu*'mol quanta m'^{-2}~'s'^{-1}*']'))
    if(j==5) mtext("Day of Year",side=1,line=2.5)
    mtext(labels[[k]],line=-1.25,adj=0.05,cex=0.8); k=k+1  

}
dev.off()

