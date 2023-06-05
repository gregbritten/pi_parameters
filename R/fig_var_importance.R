library(viridis)

source('r/plot_rf_importance.r') ##variable importance plot

##--Load fitted random forests--###########
fits_Ek_chl_T  <- readRDS('results/fits_Ek_chl_T.rds')
fits_PBmax_chl <- readRDS('results/fits_PBmax_chl_T.rds')
fits_alpha_chl <- readRDS('results/fits_alpha_chl_T.rds')

regions     <- c('scot','lab','spac','tas','ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf','Global')

##--Fit models--###################

##--Make plot--####################
#Plot setup
cols <- turbo(4)
lets <- c('a)','b)','c)','d)','e)','f)')
nms <- numeric(6)
for(i in 1:6) nms[i] <- paste(lets[i],region_long[i])


ylims=c(700,1000,3500,400,100)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_Ek_chl_T,chl=TRUE,terms=c(1,2,3,4))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
#legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'),lty=1, col=cols,bty='n',cex=1.2)
legend('topleft',legend=c('Chl',expression('Kd'['490']),'SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)

ylims=c(2.2,0.5,0.3,2,0.3)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_PBmax_chl_T,terms=c(1,2,3,4))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('Chl',expression('Kd'['490']),'SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)

ylims=c(0.00025,0.0001,0.0002,0.0003,0.00015)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_rf_importance(fits=fits_alpha_chl_T,terms=c(1,2,3,4))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('Chl',expression('Kd'['490']),'SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)
