library(viridis)

source('r/f_plot_rf_rsq.R') ##

##--Load fitted random forests--###########
fits_Ek_chl_F_365    <- readRDS('results/fits_Ek_chl_F_365.rds')
fits_PBmax_chl_F_365 <- readRDS('results/fits_PBmax_chl_F_365.rds')
fits_alpha_chl_F_365 <- readRDS('results/fits_alpha_chl_F_365.rds')


##--60 Days--##################
pdf('plots/rq_timescale_full_false_60.pdf',height=4,width=7.5)
cols <- turbo(5)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
f_plot_rf_rsq(fits_PBmax_chl_F_365, parm='PBmax',ylim_rmse=c(0.5,1.5),ylim_r2=c(0,1),text=expression(italic('P'['max']^{'B'})),outer=TRUE,
            ntime=60)
plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
f_plot_rf_rsq(fits_alpha_chl_F_365,parm='alpha',ylim_rmse=c(0.005,0.025),ylim_r2=c(0,1),text=expression(italic(alpha^{'B'})),
            ntime=60)
dev.off()


##--365 Days--##################
pdf('plots/rq_timescale_full_false_365.pdf',height=4,width=7.5)
cols <- turbo(5)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
  f_plot_rf_rsq(fits_PBmax_chl_F_365, parm='PBmax',ylim_rmse=c(0.5,1.5),ylim_r2=c(0,1),text=expression(italic('P'['max']^{'B'})),outer=TRUE,
              ntime=365,max=FALSE)
    plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
    legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
  f_plot_rf_rsq(fits_alpha_chl_F_365,parm='alpha',ylim_rmse=c(0.005,0.025),ylim_r2=c(0,1),text=expression(italic(alpha^{'B'})),
              ntime=365,max=FALSE)
dev.off()

