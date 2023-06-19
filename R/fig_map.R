library(maps)
library(viridis)

source('r/process_csvs.r') ##--data processing--###########

file <- mean_files[1]

d     <- as.data.frame(D[[1]])
d$lat <- jitter(d$lat,factor=100)
d$lon <- jitter(d$lon,factor=100)

regions <- c('scot','lab','spac','tas','ice')
cols <- turbo(5)

pdf('~/dropbox/working/pi_parameters/github/plots/map_obs.pdf',height=4.5,width=6.5)
par(mfrow=c(1,1),oma=c(2,2,2,2))
map(ylim=c(-90,90))
  box(lwd=2)
  #points(jitter(d$lon,factor=100),jitter(d$lat,factor=100),pch=19,cex=0.2,col='red')
    points(d$lon[!(d$region%in%regions)],d$lat[!(d$region%in%regions)],pch=4,cex=0.2,col='dark grey')
  for(i in 1:length(regions)){
    points(d$lon[d$region==regions[i]],d$lat[d$region==regions[i]],pch=4,cex=0.3,col=cols[i],lwd=0.5)
  }
  abline(h=seq(-90,90,10),lty=2,lwd=0.25)
  abline(v=seq(-180,180,10),lty=2,lwd=0.25)
  axis(side=2,at=seq(-90,90,10),cex.axis=0.6,las=2)
  axis(side=1,at=seq(-180,180,10),cex.axis=0.6,las=2)
  #mtext(side=1,'Latitude',line=2.5)
  #mtext(side=1,'Latitude',line=2.5)
dev.off()

