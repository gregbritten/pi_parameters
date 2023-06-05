library(viridis)

source('r/plot_rf_rsq.r') ##

##--Load fitted random forests--###########
fits_Ek_chl_T  <- readRDS('results/fits_Ek_chl_T.rds')
fits_PBmax_chl <- readRDS('results/fits_PBmax_chl_T.rds')
fits_alpha_chl <- readRDS('results/fits_alpha_chl_T.rds')

pdf('plots/rq_timescale_full_false.pdf',height=7.5,width=5.5)
cols <- turbo(5)
par(mfrow=c(3,2),mar=c(2,2,2,2),oma=c(3,3,2,2))
  plot_rf_rsq(fits_Ek_chl,parm='Ek',ylim_rmse=c(10,120),ylim_r2=c(0,1), legend=TRUE,text=expression(italic('E'['k'])))
  plot_rf_rsq(fits_PBmax_chl, parm='PBmax',ylim_rmse=c(0.5,1.5),ylim_r2=c(0,1),text=expression(italic('P'['max']^{'B'})))
  plot_rf_rsq(fits_alpha_chl,parm='alpha',ylim_rmse=c(0.005,0.025),ylim_r2=c(0,1),text=expression(italic(alpha^{'B'})))
dev.off()

