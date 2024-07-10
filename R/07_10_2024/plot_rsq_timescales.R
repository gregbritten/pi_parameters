library(viridis)

source('r/f_plot_rf_rsq.R') ##

##--Load fitted random forests--###########
fit_Ek_40    <- readRDS('results/fit_Ek_40.rds')
fit_PBmax_40 <- readRDS('results/fit_PBmax_40.rds')
fit_alpha_40 <- readRDS('results/fit_alpha_40.rds')

fit_Ek_all    <- readRDS('results/fit_Ek_all.rds')
fit_PBmax_all <- readRDS('results/fit_PBmax_all.rds')
fit_alpha_all <- readRDS('results/fit_alpha_all.rds')


#########################################################################
## 60 days ##################################################################
#########################################################################
##--40m--############################
pdf('plots/rq_timescale_40.pdf',height=3,width=10.5)
cols <- turbo(5)
par(mfrow=c(1,4),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
f_plot_rf_rsq(fit_PBmax_40, parm='PBmax', ylim_rmse=c(0.5,1.5),
                                        ylim_r2=c(0,1),
                                        text=expression(italic('P'['max']^{'B'})),outer=TRUE,
                                        ntime=30)

f_plot_rf_rsq(fit_alpha_40, parm='alpha',ylim_rmse=c(0.005,0.025),
                                       ylim_r2=c(0,1),
                                       text=expression(italic(alpha^{'B'})),
                                       ntime=30)

f_plot_rf_rsq(fit_Ek_40,    parm='Ek',ylim_rmse=c(50,200),
                                    ylim_r2=c(0,1),
                                    text=expression(italic('E'['K'])),
                                    ntime=30)

plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
dev.off()


##--all--##################
pdf('plots/rq_timescale_all.pdf',height=3,width=10.5)
cols <- turbo(5)
par(mfrow=c(1,4),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
f_plot_rf_rsq(fit_PBmax_all, parm='PBmax', ylim_rmse=c(0.5,1.5),
              ylim_r2=c(0,1),
              text=expression(italic('P'['max']^{'B'})),outer=TRUE,
              ntime=60)

f_plot_rf_rsq(fit_alpha_all, parm='alpha',ylim_rmse=c(0.005,0.025),
              ylim_r2=c(0,1),
              text=expression(italic(alpha^{'B'})),
              ntime=60)

f_plot_rf_rsq(fit_Ek_all,    parm='Ek',ylim_rmse=c(50,200),
              ylim_r2=c(0,1),
              text=expression(italic('E'['K'])),
              ntime=60)

plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
dev.off()



#########################################################################
## 365 days ##################################################################
#########################################################################
##--40m--############################
pdf('plots/rq_timescale_40.pdf',height=3,width=10.5)
cols <- turbo(5)
par(mfrow=c(1,4),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
f_plot_rf_rsq(fit_PBmax_40, parm='PBmax', ylim_rmse=c(0.5,1.5),
              ylim_r2=c(0,1),
              text=expression(italic('P'['max']^{'B'})),outer=TRUE,
              ntime=180)

f_plot_rf_rsq(fit_alpha_40, parm='alpha',ylim_rmse=c(0.005,0.025),
              ylim_r2=c(0,1),
              text=expression(italic(alpha^{'B'})),
              ntime=180)

f_plot_rf_rsq(fit_Ek_40,    parm='Ek',ylim_rmse=c(50,200),
              ylim_r2=c(0,1),
              text=expression(italic('E'['K'])),
              ntime=180)

plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
dev.off()


##--all--##################
pdf('plots/rq_timescale_all.pdf',height=3,width=10.5)
cols <- turbo(5)
par(mfrow=c(1,4),mar=c(2,2,2,2),oma=c(3,3,2,2),cex.axis=0.8)
f_plot_rf_rsq(fit_PBmax_all, parm='PBmax', ylim_rmse=c(0.5,1.5),
              ylim_r2=c(0,1),
              text=expression(italic('P'['max']^{'B'})),outer=TRUE,
              ntime=365)

f_plot_rf_rsq(fit_alpha_all, parm='alpha',ylim_rmse=c(0.005,0.025),
              ylim_r2=c(0,1),
              text=expression(italic(alpha^{'B'})),
              ntime=365)

f_plot_rf_rsq(fit_Ek_all,    parm='Ek',ylim_rmse=c(50,200),
              ylim_r2=c(0,1),
              text=expression(italic('E'['K'])),
              ntime=365)

plot(-999,bty='n',xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1))
legend('topleft',legend=region_long[1:5], lty=1, col=cols,bty='n',cex=1)
dev.off()
