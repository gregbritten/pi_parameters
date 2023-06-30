library(viridis)
library(randomForest)

source('r/plot_rf_rsq.r') ##

region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf')
##--Load fitted random forests--###########
fits_Ek_chl_T    <- readRDS('results/fits_Ek_chl_T.rds')
fits_PBmax_chl_T <- readRDS('results/fits_PBmax_chl_T.rds')
fits_alpha_chl_T <- readRDS('results/fits_alpha_chl_T.rds')

pdf('plots/rq_timescale_full_true.pdf',height=6,width=7.5)
cols <- turbo(5)
par(mfrow=c(3,3),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
  plot_rf_rsq(fits_PBmax_chl_T, parm='PBmax',ylim_rmse=c(0.5,1.5),ylim_r2=c(0,1),text=expression(italic('P'['max']^{'B'})),outer=TRUE,
              ntime=365)
    plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
    legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
  plot_rf_rsq(fits_alpha_chl_T,parm='alpha',ylim_rmse=c(0.005,0.025),ylim_r2=c(0,1),text=expression(italic(alpha^{'B'})),
              ntime=365)
    plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
  plot_rf_rsq(fits_Ek_chl_T,parm='Ek',ylim_rmse=c(10,70),ylim_r2=c(0,1),text=expression(italic('E'['k'])),
              ntime=365)
    plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
dev.off()

