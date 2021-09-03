##################################################################################
## script to generate masks for pacific, atlantic and indian basins
##################################################################################
library(ncdf4)
lons <- seq(-180,180,length.out=360)
lats <- seq(-90,90,length.out=180)

mask_nc <- nc_open('~/dropbox/data/masks/mask.nc')

pacific=pacific_open=pacific_coast  <- ncvar_get(mask_nc,'pacific')[c(181:360,1:180),]
atlantic <- ncvar_gets(mask_nc,'atlantic')[c(181:360,1:180),]
indian   <- ncvar_get(mask_nc,'indian')[c(181:360,1:180),]
SO       <- ncvar_get(mask_nc,'southern')[c(181:360,1:180),]



coasts   <- ncvar_get(mask_nc,'coast')[c(181:360,1:180),]

##--modify Pacific--#####################
pacific[pacific>0]      <- 1
pacific[SO%in%c(1,2,3)] <- 1
pacific[lons > -70 & lons < 135,lats < -30] <- NA
#pacific[coasts==1] <- NA
pacific[pacific==0] <- NA

##--modify Atlantic--#####################
atlantic[atlantic==6] <- 0
atlantic[atlantic>0] <- 1
atlantic[SO%in%c(1,2,3)] <- 1
atlantic[lons < -70,lats < -30] <- NA
atlantic[lons > 20,lats < -30] <- NA
#atlantic[coasts==1] <- NA
atlantic[atlantic==0] <- NA


##--modify Indian--#####################
indian[indian>0] <- 1
indian[SO%in%c(1,2,3)] <- 1
indian[lons < 20, lats<30] <- 0
indian[lons >135, lats<30] <- 0
#indian[coasts==1] <- NA
indian[indian==0] <- NA
##--put them all together--#####################
basins <- pacific
basins[atlantic==1] <- 2
basins[indian==1] <- 3

#rm(list=c('pacific','atlantic','indian'))
