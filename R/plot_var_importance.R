library(viridis)
library(corrplot)

source('r/f_plot_rf_importance.r') ##variable importance plot

##--Load fitted random forests--###########
fits_Ek_chl_F_365    <- readRDS('results/fits_Ek_chl_T_365.rds')
fits_PBmax_chl_F_365 <- readRDS('results/fits_PBmax_chl_T_365.rds')
fits_alpha_chl_F_365 <- readRDS('results/fits_alpha_chl_T_365.rds')

regions     <- c('scot','lab','spac','tas','ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf','Global')

##--Make plot--####################
#Plot setup
lets <- c('a)','b)','c)','d)','e)','f)')
nms <- numeric(6)
for(i in 1:6) nms[i] <- paste(lets[i],region_long[i])
cols <- c(turbo(4)[c(3,4)],'dark green','black','purple')

##--60 Days--##########
pdf('plots/variable_importance_60.pdf',height=2.75*(3/2),width=8)
#pdf('plots/variable_importance_60_other_var.pdf',height=2.75*(3/2),width=9)
ylims=rep(0.5,6)
par(mfrow=c(3,7),mar=c(1.5,2,0,0),oma=c(3,3,3,3),cex.axis=0.7)
f_plot_rf_importance(fits=fits_PBmax_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),labs=TRUE,ntime=60)
#f_plot_rf_importance(fits=fits_PBmax_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),labs=TRUE,ntime=60)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  legend('topleft',legend=c('PAR','SST','Chl'),lty=1, col=cols,bty='n',cex=1.5)
  #legend('topleft',legend=c('PAR','SST','Chl','nano/pico','micro/nano'),lty=1, col=cols,bty='n',cex=1.2)
  mtext(expression(italic('P'['max']^'B')),side=1,adj=0)
f_plot_rf_importance(fits=fits_alpha_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),ntime=60)
#f_plot_rf_importance(fits=fits_alpha_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),ntime=60)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(expression(italic(alpha^'B')),side=1,line=-3,adj=0)
f_plot_rf_importance(fits=fits_Ek_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),xaxt=TRUE,ntime=60)
#f_plot_rf_importance(fits=fits_Ek_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),xaxt=TRUE,ntime=60)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(expression(italic('E'['K'])),side=1,line=-3,adj=0)
  mtext(outer=TRUE,'Relative RMSE Increase',side=2,line=0.5)
  mtext(outer=TRUE,expression('Initial Time '*italic('t'['i'])),side=1,line=1.5)
dev.off()

##--365 Days--######################
#pdf('plots/variable_importance_365.pdf',height=4,width=11)
pdf('plots/variable_importance_365.pdf',height=2.75*(3/2),width=8)
par(mfrow=c(3,7),mar=c(1.5,2,0,0),oma=c(3,3,3,3),cex.axis=0.7)
#ylims=c(700,1000,3500,400,100)
ylims=rep(0.5,6)
#ylims=c(2.2,0.5,0.3,2,0.3)
f_plot_rf_importance(fits=fits_PBmax_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),labs=TRUE,ntime=365)
#f_plot_rf_importance(fits=fits_PBmax_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),labs=TRUE,ntime=365)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(expression(italic('P'['max']^'B')),side=1)
  legend('topleft',legend=c('PAR','SST','Chl'),lty=1, col=cols,bty='n',cex=1.5)
  #legend('topleft',legend=c('PAR','SST','Chl','nano/pico','micro/nano'),lty=1, col=cols,bty='n',cex=1.2)
#ylims=c(0.00025,0.0001,0.0002,0.0003,0.00015)
f_plot_rf_importance(fits=fits_alpha_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),ntime=365)
#f_plot_rf_importance(fits=fits_alpha_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),ntime=365)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(expression(italic(alpha^'B')),side=1,line=-3)
f_plot_rf_importance(fits=fits_Ek_chl_F_365,chl=TRUE,terms=c('par','sst','chl'),ntime=365,xaxt=TRUE)
#f_plot_rf_importance(fits=fits_Ek_chl_F_365,chl=TRUE,terms=c('par','sst','chl','nano_pico','micro_nano'),ntime=365,xaxt=TRUE)
  plot(-999,xaxt='n',yaxt='n',xlim=c(0,1),ylim=c(0,1),bty='n')
  mtext(expression(italic('E'['K'])),side=1)
  mtext(outer=TRUE,'Relative RMSE Increase',side=2,line=0.5)
  mtext(outer=TRUE,expression('Initial Time '*italic('t'['i'])),side=1,line=1.5)
dev.off()

