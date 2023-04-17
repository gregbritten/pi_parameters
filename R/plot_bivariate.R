
library(randomForest)
library(car)

pdf('~/dropbox/working/pi_parameters/github/plots/north_atlantic_points_04_13_2023.pdf',height=5.5,width=4.5)
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
  maps::map(fill=TRUE,col='grey',xlim=c(-100,-20),ylim=c(30,80),resolution=0)
  points(nwa_d$lon,nwa_d$lat,cex=0.2,col='red',pch=4,lwd=0.5)
  box()
  axis(side=1); axis(side=2)
  mtext(side=1,line=2.5,"Longitude")
  mtext(side=2,line=2.5,"Latitude")
dev.off()

##--PLOT BIVARIATE RELATIONSHIPS--######################################
pdf('~/dropbox/working/pi_parameters/github/plots/pmb_alpha_ek_scatter_atl.pdf',height=4,width=9.5)
par(mfcol=c(2,6),mar=c(0,0,0,0),oma=c(6,6,4,4),cex.axis=0.8)
  #fit1  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=nwatl, start=list(a=2.5,b=0.06,z=13,w=14))
  #fit2  <- lm(PMB ~ TEMP + I(TEMP^2) + I(TEMP^3), data=nwatl) 
plot(nwa_d$temp,nwa_d$PBmax,cex=0.3,xlim=c(-2,30),ylim=c(0,15),xaxt='n')
  mtext(expression('P'['m']^{'B'}),side=2,line=2.5)
  #lines(seq(0,30,0.01),predict(fit1,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
  #lines(seq(0,30,0.01),predict(fit2,newdata=list(TEMP=seq(0,30,0.01))),col='green')  
  #lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  
  #legend(legend=c('BF97','Epply-like', '3rd ord. poly'),'topleft',lty=1,col=c('blue','red','green'),bty='n')
plot(nwa_d$temp,nwa_d$alpha,xlim=c(-2,30),cex=0.3,ylim=c(0,0.21))
mtext(expression(alpha^{'B'}),side=2,line=2.5)
mtext('Temperature',side=1,line=2.5)

plot(nwa_d$sst,nwa_d$PBmax,cex=0.3,xlim=c(-2,30),ylim=c(0,15),xaxt˜='n')
plot(nwa_d$sst,nwa_d$alpha,xlim=c(-2,30),cex=0.3,ylim=c(0,0.21))
#mtext('Temperature',side=1,line=2.5)

plot(nwa_m$sst,nwa_m$PBmax,cex=0.3,xlim=c(-2,30),ylim=c(0,15),xaxt='n')
plot(nwa_m$sst,nwa_m$alpha,xlim=c(-2,30),cex=0.3,ylim=c(0,0.21))

plot(nwa_d$depth,nwa_d$PBmax,cex=0.3,ylim=c(0,15),yaxt='n',xaxt='n',xlim=c(0,130))
plot(nwa_d$depth,nwa_d$alpha,cex=0.3,ylim=c(0,0.21),yaxt='n',xlim=c(0,130))

plot(nwa_m$depth,nwa_m$PBmax,cex=0.3,ylim=c(0,15),yaxt='n',xaxt='n',xlim=c(0,130))
plot(nwa_m$depth,nwa_m$alpha,cex=0.3,ylim=c(0,0.21),yaxt='n',xlim=c(0,130))



plot(log10(nwatl$NITRATE+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.07,20)),ylim=c(0,15),xaxt='n',yaxt='n')
plot(log10(nwatl$NITRATE+0.1),nwatl$ALPHA,xlim=log10(c(0.07,20)),cex=0.3,ylim=c(0,0.21),yaxt='n')
mtext(expression('log'['10']~'(Nitrate)'),side=1,line=2.5)
plot(log10(nwatl$TCHL+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.1,25)),ylim=c(0,15),xaxt='n',yaxt='n')
plot(log10(nwatl$TCHL+0.1),nwatl$ALPHA,xlim=log10(c(0.1,25)),cex=0.3,ylim=c(0,0.21),yaxt='n')
mtext(expression('log'['10']~'(Chlorophyll)'),side=1,line=2.5)
plot(nwatl$DEPTH,nwatl$PMB,cex=0.3,ylim=c(0,15),yaxt='n',xaxt='n',xlim=c(0,130))
plot(nwatl$DEPTH,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),yaxt='n',xlim=c(0,10))
mtext('Depth',side=1,line=2.5)
plot(nwatl$PARc,nwatl$PMB,cex=0.3,ylim=c(0,15),xlim=c(0,60),xaxt='n',yaxt='n')
plot(nwatl$PARc,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),xlim=c(0,60),yaxt='n')
mtext('PAR',side=1,line=2.5)
plot(nwatl$DEPTH*nwatl$KDc,nwatl$PMB,cex=0.3,ylim=c(0,15),xlim=c(0,6),xaxt='n',yaxt='n')
plot(nwatl$DEPTH*nwatl$KDc,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),xlim=c(0,6),yaxt='n')
mtext('Optical Depth',side=1,line=2.5)
dev.off()



