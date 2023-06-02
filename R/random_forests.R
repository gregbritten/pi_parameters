#library(mgcv)
library(randomForest)
library(viridis)
#library(doParallel)
#ncores <- detectCores()      
#registerDoParallel(ncores-1) #leave one core for other tasks

source('r/fit_rf.r') ##--data processing--###########
source('r/plot_rf_importance.r') ##--data processing--###########
source('r/plot_rf_rsq.r') ##--data processing--###########

regions     <- c('scot','lab','spac','tas','ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf','Global')

##--Fit models--###################

##--Make plot--####################
#Plot setup
cols <- turbo(9)
lets <- c('a)','b)','c)','d)','e)','f)')
nms <- numeric(6)
for(i in 1:6) nms[i] <- paste(lets[i],region_long[i])

ylims=c(1000,1000,4000,1000,150)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_Ek_chl,chl=TRUE)
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), 
       lty=1, col=cols,bty='n',cex=1.2)

ylims=c(2.2,0.5,0.5,2,0.5)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_PBmax_chl,chl=TRUE)
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), 
       lty=1, col=cols,bty='n',cex=1.2)

ylims=c(0.0003,0.0001,0.0005,0.0006,0.0002)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_alpha_chl,chl=TRUE)
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'), 
       lty=1, col=cols,bty='n',cex=1.2)


cols <- turbo(6)
pdf('plots/rq_timescale.pdf',height=7.5,width=5.5)
par(mfrow=c(3,2),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_rsq(fits_Ek_chl,parm='Ek',ylim_rmse=c(10,70),legend=TRUE,text=expression(italic('E'['k'])))
plot_rf_rsq(fits_PBmax_chl, parm='PBmax',ylim_rmse=c(0.5,1.5),text=expression(italic('P'['max']^{'B'})))
plot_rf_rsq(fits_alpha_chl,parm='alpha',ylim_rmse=c(0.005,0.025),text=expression(italic(alpha^{'B'})))
dev.off()

