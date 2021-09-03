
library(readxl)
library(rgdal)
library(maptools)
library(maps)
library(raster)
library(tidyverse)

d <- read_excel('~/google/WORKING/pi_parameters/data/PE_MAPP_2020_HAB_GKU_ADD-DATA_14062021.xlsx',col_types='numeric')

##############################################################
##--AMT CRUISE--##############################################
##############################################################
##--MAP--##########
amt <- d[7209:7411,]

pdf('~/google/working/pi_parameters/plots/AMT_map.pdf',height=7,width=7)
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
maps::map(fill=TRUE,col='grey',ylim=c(-90,90),xlim=c(-100,30))
points(amt$LON,amt$LAT,cex=0.5,col='red')
axis(side=1)
axis(side=2,at=c(-90,-45,0,45,90))
# axis(side=1,at=c(-180,-90,0,90,180))
# axis(side=2,at=c(-90,-45,0,45,90))
#mtext('AMT Transect')
dev.off()


##--ALPHA, PMB, EK vs. LATITUDE--##########
pdf('~/google/working/pi_parameters/plots/AMT_parameters_lat_depth.pdf',height=6,width=8)
order = findInterval(amt$DEPTH, sort(amt$DEPTH))
par(mfrow=c(3,1),mar=c(2,2,2,8),oma=c(3,3,3,3))
plot(amt$LAT,amt$PMB,col=viridis(nrow(amt))[order],pch=19,xlim=c(-55,55))
  mtext(side=2,bquote('P'['m']^'B'),line=2.5)
image.plot(legend.only=TRUE, matrix(amt$DEPTH),col=viridis(20))
plot(amt$LAT,amt$ALPHA,col=viridis(nrow(amt))[order],pch=19,xlim=c(-55,55))
  mtext(side=2,bquote(alpha^'B'),line=2.5)
plot(amt$LAT,amt$EK,col=viridis(nrow(amt))[order],pch=19,xlim=c(-55,55))
  mtext(side=2,bquote('E'['k']),line=2.5)
mtext('Latitude',side=1,line=2.5)
dev.off()

#######################################################
##--GLOBAL--###########################################
#######################################################
##--MAP--##########
pdf('~/google/working/pi_parameters/plots/covariates_n.pdf',height=5,width=9)
par(mfrow=c(2,2),mar=c(0,0,0,0),oma=c(0,0,0,0))
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(d$LON,d$LAT,cex=0.2,col='red')
mtext(bquote('All Points  '~italic('n')~'='~.(nrow(d))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- d[complete.cases(d$TEMP),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature  '~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- dd[complete.cases(dd$NITRATE),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature & Nitrate'~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- dd[complete.cases(dd$PAR_INSITU),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature & Nitrate & Irradiance'~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dev.off()

######################################################################
##--NORTHWEST ATLANTIC--##############################################
######################################################################
##--MAP--##########
nwatl <- d[d$LAT > 35 & d$LAT < 70 & d$LON < -35 & d$LON >-80,]

pdf('~/google/working/pi_parameters/plots/north_atlantic_points.pdf',height=6,width=5)
par(mfrow=c(1,1))
maps::map(fill=TRUE,col='grey',xlim=c(-90,-20),ylim=c(25,80))
points(nwatl$LON,nwatl$LAT,cex=0.2,col='red')
axis(side=1); axis(side=2)
dev.off()

plot(nwatl$LAT)

##--seasonal decomposition--###############
seas <- list()
seas[['winter']] <- c(12,1,2)
seas[['spring']] <- c(3,4,5)
seas[['summer']] <- c(6,7,8)
seas[['fall']]   <- c(9,10,11)

order     <- findInterval(nwatl$DEPTH, sort(nwatl$DEPTH))

pdf('~/google/working/pi_parameters/plots/NWAtl_parameters_depth_lat_season.pdf',height=7,width=11)
par(mfcol=c(3,4),mar=c(2,3,2,2),oma=c(3,3,3,3))
for(i in 1:4){
  nwatl_tmp <- nwatl[nwatl$MONTH %in% seas[[i]],]
  order     <- findInterval(c(nwatl_tmp$DEPTH,max(nwatl$DEPTH,na.rm=TRUE)), sort(c(nwatl_tmp$DEPTH,max(nwatl$DEPTH,na.rm=TRUE))))
  #plot(nwatl_tmp$LAT,nwatl_tmp$PMB,col=viridis(nrow(nwatl_tmp))[order],pch=19,xlim=c(35,70),ylim=c(0,20))
  plot(nwatl_tmp$LAT,nwatl_tmp$PMB,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,20),cex=0.5)
  mtext(side=2,bquote(italic('P'['m']^'B')),line=2.5)
  mtext(names(seas[i]))
  plot(nwatl_tmp$LAT,nwatl_tmp$ALPHA,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,0.4),cex=0.5)
  mtext(side=2,bquote(alpha^'B'),line=2.5)
  plot(nwatl_tmp$LAT,nwatl_tmp$EK,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,700),cex=0.5)
  mtext(side=2,bquote('E'['k']),line=2.5)
  mtext('Latitude',side=1,line=2.5)
}
image.plot(legend.only=TRUE, add=TRUE,matrix(nwatl$DEPTH),col=viridis(20))
dev.off()

plot(nwatl$NITRATE,nwatl$PMB,xlim=c(0,20))
plot(nwatl$NITRATE,nwatl$ALPHA,xlim=c(0,20),ylim=c(0,0.2))


order = findInterval(nwatl$DEPTH, sort(nwatl$DEPTH))
nwatl$col <- viridis(nrow(nwatl))[order]

pdf('~/google/working/pi_parameters/plots/NWAtl_parameters_depth_lat.pdf',height=7,width=7)
par(mfrow=c(3,1),mar=c(2,2,2,8),oma=c(3,3,3,3))
#plot(nwatl$LAT,nwatl$PMB,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
plot(nwatl$LAT,nwatl$PMB,col=nwatl$col,pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('P'['m']^'B'),line=2.5)
image.plot(legend.only=TRUE, matrix(nwatl$DEPTH),col=viridis(20))
plot(nwatl$LAT,nwatl$ALPHA,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote(alpha^'B'),line=2.5)
plot(nwatl$LAT,nwatl$EK,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('E'['k']),line=2.5)
mtext('Latitude',side=1,line=2.5)
dev.off()

##########################################################################
##--PRELIMINARY MODEL FITS--##############################################
##########################################################################
library(mgcv)
summary(lm(PMB ~ TEMP + DEPTH + NITRATE + LAT,data=amt))

fit_PMB   <- gam(PMB ~ s(TEMP) + s(DEPTH) + s(NITRATE) + s(MLD),data=amt)
fit_alpha <- gam(ALPHA ~ s(TEMP) + s(DEPTH) + s(NITRATE) + s(MLD),data=amt)

plot(amt$PMB, predict(fit_PMB,newdata=amt))
plot(amt$ALPHA, predict(fit_alpha,newdata=amt))


#####################################################################
##--PROVINCES--######################################################
#####################################################################
longhurst <- readOGR('~/google/WORKING/pi_parameters/data/longhurst_v4_2010/Longhurst_world_v4_2010.shp')
longhurst <- shapefile('~/google/WORKING/pi_parameters/data/longhurst_v4_2010/Longhurst_world_v4_2010.shp')

