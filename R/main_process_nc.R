library(ncdf4)
library(tidyverse)

source('R/brewin.R')

regions     <- c('scot',         'lab',         'spac',         'tas',           'ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf')

nc <- nc_open('~/dropbox/working/pi_parameters/mapps_forest/upstream_files/mapps_upstream_sat_365.nc')

sstnc <- ncvar_get(nc,'sst')
parnc <- ncvar_get(nc,'par')
chlnc <- ncvar_get(nc,'chl')

lat       <- ncvar_get(nc, 'lat')
lon       <- ncvar_get(nc, 'lon')
depth     <- ncvar_get(nc, 'depth') 
month     <- ncvar_get(nc, 'month') 

ID   <- ncvar_get(nc, "ID")
date <- ncvar_get(nc, 'date')

PBmax <- ncvar_get(nc, 'PBmax')
alpha <- ncvar_get(nc, 'alpha')
Ek    <- ncvar_get(nc, 'Ek')

region = ifelse(lat> -40 & lat<0 & lon> -150 & lon< -70,'spac',
                ifelse(lat> -60 & lat< -40 & lon< 160 & lon>130,'tas',
                       ifelse(lat> 30 & lat<50 & lon> -70 & lon< -40,'scot',
                              ifelse(lat>= 50 & lat<65 & lon> -60 & lon< -40,'lab',
                                     ifelse(lat>= 58 & lat<72 & lon> -35 & lon< 0,'ice',NA)))))

##--Filter PAR outliers due NASA data issue around coast--##################
ice_i <- which(region=="ice")
for(i in 1:length(ice_i)){
  xx   <- parnc[,ice_i[i]]
  fit  <- loess.smooth(y=xx, x=1:366, span=0.3, evaluation=366)
  pred <- fit$y
  res  <- xx/abs(pred)
  parnc[which(res>5),ice_i[i]] <- pred[which(res>5)] 
}

##--Create dataset for analysis--#####################
D_nc <- list()
D_nc[[1]] <- data.frame(sst=sstnc[1,],
                        chl=chlnc[1,],
                        par=parnc[1,],
                        nano_pico=sapply(1:1954,function(x) nano_pico(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        micro_nano=sapply(1:1954,function(x) micro_nano(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        lat=lat,lon=lon,depth=depth,
                        month=month,
                        PBmax=PBmax,alpha=alpha,Ek=Ek,
                        region=region,
                        date=date[1,])

##--print dataset for comparison--######################

dat <- D_nc[[1]] %>% 
  mutate(date=as.Date(date,origin="1998-03-20")) %>%
  filter(region %in% regions)
write.csv(file='dat.csv',dat,row.names=FALSE)


for(i in 2:365){
  sst=colMeans(sstnc[1:i,],na.rm=TRUE)
  chl=colMeans(chlnc[1:i,],na.rm=TRUE)
  par=colMeans(parnc[1:i,],na.rm=TRUE)
  D_nc[[i]] <- data.frame(sst=sst,
                          chl=chl,
                          par=par,
                          nano_pico=sapply(1:1954,function(x) nano_pico(G,H,J,K,C=chl[x],SST=sst[x])),
                          micro_nano=sapply(1:1954,function(x) micro_nano(G,H,J,K,C=chl[x],SST=sst[x])),
                          lat=lat,lon=lon,depth=depth,
                          month=month,
                          PBmax=PBmax,alpha=alpha,Ek=Ek,
                          region=region,
                          date=date[1,])
}

#D_nc_365 <- list()
#D_nc_365[[1]] <- data.frame(sst=sst2[1,],chl=chl2[1,],par=par2[1,],
#                            lat=lat,lon=lon,depth=depth,
#                            month=month,
#                            PBmax=PBmax,alpha=alpha,Ek=Ek,
#                            region=region)
#for(i in 2:365){
#  D_nc_365[[i]] <- data.frame(sst=t(sst2[1:i,]),
#                              chl=t(chl2[1:i,]),
#                              par=t(par2[1:i,]),
#                              lat=lat,lon=lon,depth=depth,
#                              month=month,
#                              PBmax=PBmax,alpha=alpha,Ek=Ek,
#                              region=region)
#}
