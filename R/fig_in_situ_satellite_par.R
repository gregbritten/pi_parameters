
regions <- c('scot','lab','spac','tas','ice')

factor <- 16
ylims  <- c(0,1400)
xlims  <- c(0,1400)

pdf('plots/in_situ_satellite_par.pdf',height=5,width=8)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(3,3,2,2))
for(j in 1:5){
  d     <- D[[1]] %>% filter(region==regions[j])#,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
  plot(d$E_surf,d$par*factor,xlim=xlims,ylim=ylims,pch=19,cex=0.5,xlab='',ylab='')
  abline(0,1,lty=2)
  mtext(adj=0,paste0(letters[j],') ',region_long[j]))
  mtext(adj=0.05,line=-1.5,paste0('RMSE = ',round(sqrt(mean((d$E_surf-d$par*factor)^2,na.rm=TRUE)),1)))
  mtext(adj=0.05,line=-3,paste0('r = ',round(cor(d$E_surf,d$par*factor,use="complete.obs"),2)))
}
mtext(side=1,outer=TRUE,expression('In-situ surface PAR ['*mu*'Einsteins/m'^2*'/s]'),line=1)
mtext(side=2,outer=TRUE,expression('Satellite surface PAR ['*mu*'Einsteins/m'^2*'/s]'),line=1)
dev.off()

