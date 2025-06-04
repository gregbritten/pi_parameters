library(ncdf4)
library(tidyverse)
library(here)

source('R/brewin.R')

regions     <- c('scot',         'lab',         'spac',         'tas',           'ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf')

nc <- nc_open('~/dropbox/working/pi_parameters/mapps_forest/upstream_files/mapps_upstream_sat_365.nc')

sstnc <- ncvar_get(nc,'sst')
parnc <- ncvar_get(nc,'par')
chlnc <- ncvar_get(nc,'chl')
kd_490<- ncvar_get(nc,'kd_490')

lat       <- ncvar_get(nc, 'lat')
lon       <- ncvar_get(nc, 'lon')
depth     <- ncvar_get(nc, 'depth') 
month     <- ncvar_get(nc, 'month') 

ID   <- ncvar_get(nc, "ID")
date <- ncvar_get(nc, 'date')

PBmax <- ncvar_get(nc, 'PBmax')
alpha <- ncvar_get(nc, 'alpha')
Ek    <- ncvar_get(nc, 'Ek')

mld_nc  <- nc_open('de_Boyer_mld.nc')
mld_mat <- ncvar_get(mld_nc,'mld_dr003')
mld_lat <- ncvar_get(mld_nc,'lat')
mld_lon <- ncvar_get(mld_nc,'lon')

#attach MLD
mld <- numeric(length(lat))
for(i in 1:length(lat)){
  lat_dist <- (mld_lat - lat[i])^2
  lon_dist <- (mld_lon - lon[i])^2

  i_lat <- which(lat_dist == min(lat_dist))[1]
  i_lon <- which(lon_dist == min(lon_dist))[1]

  mld[i] <- mld_mat[i_lon,i_lat,month[i]]
}

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

#compute mixed layer average light
MLD <- matrix(NA,nrow=366,ncol=length(lat))
for(i in 1:366) MLD[i,]=mld
parmlnc <- (1/(kd_490*MLD))*parnc*exp(-kd_490*MLD)*(1-exp(-kd_490*MLD))

##--Create dataset for analysis--#####################
D_nc <- list()
D_nc[[1]] <- data.frame(sst=sstnc[1,],
                        chl=chlnc[1,],
                        par=parnc[1,],
                        parml=parmlnc[1,],
                        nano_pico=sapply(1:1954,function(x) nano_pico(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        micro_nano=sapply(1:1954,function(x) micro_nano(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        pico=sapply(1:1954,function(x) pico(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        micro_pico=sapply(1:1954,function(x) micro_pico(G,H,J,K,C=chlnc[1,x],SST=sstnc[1,x])),
                        lat=lat,lon=lon,depth=depth,
                        month=month,
                        PBmax=PBmax,alpha=alpha,Ek=Ek,
                        region=region,
                        date=date[1,],
                        mld=mld)

##--print dataset for comparison--######################
dat <- D_nc[[1]] %>% 
  mutate(date=as.Date(date,origin="1998-03-20")) %>%
  filter(region %in% regions)
write.csv(file='dat.csv',dat,row.names=FALSE)


for(i in 2:365){
print(i)
  sst=colMeans(sstnc[1:i,],na.rm=TRUE)
  chl=colMeans(chlnc[1:i,],na.rm=TRUE)
  par=colMeans(parnc[1:i,],na.rm=TRUE)
  parml=colMeans(parmlnc[1:i,],na.rm=TRUE)

  D_nc[[i]] <- data.frame(sst=sst,
                          chl=chl,
                          par=par,
                          parml=parml,
                          nano_pico=sapply(1:1954,function(x) nano_pico(G,H,J,K,C=chl[x],SST=sst[x])),
                          micro_nano=sapply(1:1954,function(x) micro_nano(G,H,J,K,C=chl[x],SST=sst[x])),
                          pico=sapply(1:1954,function(x) pico(G,H,J,K,C=chl[x],SST=sst[x])),
                          micro_pico=sapply(1:1954,function(x) micro_pico(G,H,J,K,C=chl[x],SST=sst[x])),
                          lat=lat,lon=lon,depth=depth,
                          month=month,
                          PBmax=PBmax,alpha=alpha,Ek=Ek,
                          region=region,
                          date=date[1,],
                          mld=mld)
}

