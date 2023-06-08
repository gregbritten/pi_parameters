library(viridis)
library(corrplot)

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
lets <- c('a)','b)','c)','d)','e)','f)')
nms <- numeric(6)
for(i in 1:6) nms[i] <- paste(lets[i],region_long[i])

pdf('plots/variable_importance_all.pdf',height=5.5,width=11)
cols <- turbo(3)[c(2,1,3)]
#ylims=c(700,1000,3500,400,100)
ylims=rep(1,5)
par(mfrow=c(3,6),mar=c(1.5,2,0,0),oma=c(3,3,3,3),cex.axis=0.7)
#ylims=c(2.2,0.5,0.3,2,0.3)
plot_rf_importance(fits=fits_PBmax_chl_T,chl=TRUE,terms=c('chl','sst','par'),labs=TRUE)
mtext(expression(italic('P'['max']^'B')),side=1)
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
legend('topleft',legend=c('Chl','SST','PAR'),lty=1, col=cols,bty='n',cex=1.5)

#ylims=c(0.00025,0.0001,0.0002,0.0003,0.00015)
plot_rf_importance(fits=fits_alpha_chl_T,chl=TRUE,terms=c('chl','sst','par'))
  mtext(expression(italic(alpha^'B')),side=1)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')

plot_rf_importance(fits=fits_Ek_chl_T,chl=TRUE,terms=c('chl','sst','par'),xaxt=TRUE)
  mtext(expression(italic('E'['k'])),side=1)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(outer=TRUE,'Relative RMSE Increase',side=2,line=0.5)
  mtext(outer=TRUE,expression('Initial Time '*italic('t'['i'])),side=1,line=1.5)

dev.off()


pdf('plots/variable_importance_correlations.pdf',height=5.5,width=12)
par(mfrow=c(1,6),mar=c(3,3,3,3),oma=c(3,3,3,3),cex.axis=0.7)
plot_rf_importance_cor(fits=fits_Ek_chl_T,chl=TRUE,terms=c('chl','sst','par'),labs=TRUE)
dev.off()






ylims=rep(5,5)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_lm_importance(fits=fits_Ek_chl_T_lm,chl=TRUE,terms=c('chl','sst','par'))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
#legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'),lty=1, col=cols,bty='n',cex=1.2)
legend('topleft',legend=c('Chl','SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)

par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_lm_importance(fits=fits_PBmax_chl_T_lm,chl=TRUE,terms=c('chl','sst','par'))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
#legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'),lty=1, col=cols,bty='n',cex=1.2)
legend('topleft',legend=c('Chl','SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)

par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
plot_lm_importance(fits=fits_alpha_chl_T_lm,chl=TRUE,terms=c('chl','sst','par'))
plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
#legend('topleft',legend=c('chl','kd_490','sst','par','lat','lon','depth','month','daylength'),lty=1, col=cols,bty='n',cex=1.2)
legend('topleft',legend=c('Chl','SST','PAR'),lty=1, col=cols,bty='n',cex=1.2)

