
library(randomForest)
library(car)

pdf('~/dropbox/working/pi_parameters/github/plots/north_atlantic_points_04_13_2023.pdf',height=5.5,width=4.5)
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
  maps::map(fill=TRUE,col='grey',xlim=c(-100,-20),ylim=c(30,80),resolution=0)
  points(nwatl$lon,nwatl$lat,cex=0.2,col='red',pch=4,lwd=0.5)
  box()
  axis(side=1); axis(side=2)
  mtext(side=1,line=2.5,"Longitude")
  mtext(side=2,line=2.5,"Latitude")
dev.off()
