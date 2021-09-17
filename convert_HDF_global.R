rm(list=ls())

library(rgdal)
library(fields)
library(viridis)

d <- read_excel('~/google/WORKING/pi_parameters/data/PE_MAPP_2020_HAB_GKU_ADD-DATA_14062021.xlsx',col_types='numeric')

source('~/dropbox/code/functions/resize_bilinear().r')

nlon <- 2160
nlat <- 1080

lons <- seq(-180,180,length.out=nlon)
lats <- seq(-90,90,length.out=nlat)

lati <- which(lats >= 30  & lats <= 70)
loni <- which(lons <= -30 & lons >=-80)
#####################################################################################
## PROCESS VARIABLES ################################################################
#####################################################################################
#variables: PAR, MLD, KD

setwd('~/dropbox/data/ocean_productivity/par')
#setwd('~/dropbox/data/ocean_productivity/mld')
#setwd('~/dropbox/data/ocean_productivity/k490')

par_files  <- list.files('~/dropbox/data/ocean_productivity/par',  full.names=TRUE)
mld_files  <- list.files('~/dropbox/data/ocean_productivity/mld',  full.names=TRUE)
kd_files   <- list.files('~/dropbox/data/ocean_productivity/k490', full.names=TRUE)

#Nfiles <- length(files)
Nfiles <- 806

#DAT  <- array(NA,dim=c(360*2,180*2,Nfiles))
Iatl=PARatl=MLDatl=kdatl <- array(NA,dim=c(length(lons[loni]),length(lats[lati]),Nfiles))
Iglb=PARglb=MLDglb=kdglb <- array(NA,dim=c(nlon/3,nlat/3,Nfiles))
date <- numeric(Nfiles)

for(i in 1:Nfiles){
  print(i)
  par_file <- par_files[i]
  mld_file <- mld_files[i]
  kd_file  <- kd_files[i]
  
  yr       <- as.numeric(substr(par_file,63,66))
  doy      <- as.numeric(substr(par_file,67,69))
  date[i]  <- as.character(as.Date(doy, origin = paste(yr,"01","01",sep='-')))
  
  # par_tmp <- as.matrix(readGDAL(par_file))[,1080:1]
  # mld_tmp <- as.matrix(readGDAL(mld_file))[,1080:1]
  # kd_tmp  <- as.matrix(readGDAL(kd_file))[,1080:1]
  # 
  # par_tmp[par_tmp==-9999] <- NA
  # mld_tmp[mld_tmp==-9999] <- NA
  # kd_tmp[kd_tmp==-9999]   <- NA
  # 
  # kd_tmp[is.na(mld_tmp)] <- NA
  # par_tmp[is.na(mld_tmp)] <- NA
  # 
  # par_tmp_atl <- par_tmp[loni,lati]
  # mld_tmp_atl <- mld_tmp[loni,lati]
  # kd_tmp_atl  <- kd_tmp[loni,lati]
  # 
  # par      <- resize_bilinear(par_tmp,xin=2160,xout=720,yin=1080,yout=180*2)
  # mld      <- resize_bilinear(mld_tmp,xin=2160,xout=720,yin=1080,yout=180*2)
  # kd       <- resize_bilinear(kd_tmp, xin=2160,xout=720,yin=1080,yout=180*2)
  # 
  # PARglb[,,i] <- par
  # MLDglb[,,i] <- mld
  # kdglb[,,i]  <- kd
  # Iglb[,,i] <- (1/(kd*mld))*par*exp(-(kd*mld))*(1-exp(-(kd*mld)))
  # 
  # PARatl[,,i] <- par_tmp_atl
  # MLDatl[,,i] <- mld_tmp_atl
  # kdatl[,,i]  <- kd_tmp_atl
  # Iatl[,,i] <- (1/(kd_tmp_atl*mld_tmp_atl))*par_tmp_atl*exp(-(kd_tmp_atl*mld_tmp_atl))*(1-exp(-(kd_tmp_atl*mld_tmp_atl)))
}


I_clim=PAR_clim=MLD_clim=kd_clim <- array(NA,dim=c(length(lons[loni]),length(lats[lati]),Nfiles))
for(i in 1:12){
print(i)
  I_clim[,,i] <- apply(Iatl[,,month(ymd(date))==i], c(1,2), function(x) mean(x, na.rm=TRUE))
}


for(i in 1:12){
  I_clim[,,i][is.na(I_clim[,,i])] <- 0
  ifelse(i!=7, I_clim[,,i][is.na(I_clim[,,7])] <- NA, I_clim[,,i][is.na(I_clim[,,6])] <- NA)
}

par(mfrow=c(4,3),mar=c(2,2,2,2))
for(i in 1:12){
  image(lons[loni],lats[lati],I_clim[,,i],zlim=c(0,15),col=viridis(20))
  d_tmp <- d[d$MONTH==i,]
  maps::map(add=TRUE)
  points(d_tmp$LON,d_tmp$LAT,pch=19,col='red',cex=0.5)
  if(i==3) image.plot(matrix(c(0,15)),legend.only=TRUE,col=viridis(20))
} 

save(file='~/dropbox/working/pi_parameters/PARglb.rdata', PARglb)	
save(file='~/dropbox/working/pi_parameters/MLDglb.rdata', MLDglb)	
save(file='~/dropbox/working/pi_parameters/kdglb.rdata',  kdglb)	
save(file='~/dropbox/working/pi_parameters/Iglb.rdata',   Iglb)	

save(file='~/dropbox/working/pi_parameters/PARatl.rdata', PARatl)	
save(file='~/dropbox/working/pi_parameters/MLDatl.rdata', MLDatl)	
save(file='~/dropbox/working/pi_parameters/kdatl.rdata',  kdatl)	
save(file='~/dropbox/working/pi_parameters/Iatl.rdata',   Iatl)	

save(file='~/dropbox/working/pi_parameters/date.rdata',   date)	





