library(maps)

source('r/process_csvs.r') ##--data processing--###########

file <- mean_files[1]

d     <- as.data.frame(D[[1]])
d$lat <- jitter(d$lat,factor=100)
d$lon <- jitter(d$lon,factor=100)


pdf('~/dropbox/working/pi_parameters/github/plots/map_obs.pdf',height=5,width=7.5)
par(oma=c(2,2,2,2))
map(ylim=c(-90,90))
  box(lwd=2)
  #points(jitter(d$lon,factor=100),jitter(d$lat,factor=100),pch=19,cex=0.2,col='red')
  points(d$lon,d$lat,pch=19,cex=0.2)
  points(d$lon[d$region=='spac'],d$lat[d$region=='spac'],pch=19,cex=0.2,col='red')
  points(d$lon[d$region=='tas'],d$lat[d$region=='tas'],pch=19,cex=0.2,col='purple')
  points(d$lon[d$region=='scot'],d$lat[d$region=='scot'],pch=19,cex=0.2,col='orange')
  points(d$lon[d$region=='lab'],d$lat[d$region=='lab'],pch=19,cex=0.2,col='blue')
  points(d$lon[d$region=='ice'],d$lat[d$region=='ice'],pch=19,cex=0.2,col='dark green')
  abline(h=seq(-90,90,10),lty=2,lwd=0.25)
  abline(v=seq(-180,180,10),lty=2,lwd=0.25)
  axis(side=2,at=seq(-90,90,10),cex.axis=0.6,las=2)
  axis(side=1,at=seq(-180,180,10),cex.axis=0.6,las=2)
  #mtext(side=1,'Latitude',line=2.5)
  #mtext(side=1,'Latitude',line=2.5)
dev.off()

