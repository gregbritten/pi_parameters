library(lubridate)

rm(list=ls())

nlon <- 720
nlat <- 360

lons <- seq(-180,180,length.out=nlon)
lats <- seq(-90,90,length.out=nlat)

google_dir <- '~/google/WORKING/pi_parameters/'

d <- read.csv(paste0(google_dir,'data/PE_MAPP_2020_HAB_GKU_ADD-DATA_14062021.csv'),stringsAsFactors=FALSE) %>% 
  mutate_all(function(x) as.numeric(as.character(x)))

load('processed_data/PAR.rds')	
load('processed_data/MLD.rds')	
load('processed_data/KD.rds')	
load('processed_data/I.rds')	
load('processed_data/CHL_GIOP.rds')	
load('processed_data/CHL_GSM.rds')	
load('processed_data/BBP_GIOP.rds')	
load('processed_data/BBP_GSM.rds')	
load('processed_data/date.rds')	
load('processed_data/time.rds')	

d$PAR  = d$MLD  = d$KD  = d$I  = d$CHL_GIOP  = d$CHL_GSM  = d$BBP_GIOP  = d$BBP_GSM  = d$OPT  <- NA
d$PARc = d$MLDc = d$KDc = d$Ic = d$CHL_GIOPc = d$CHL_GSMc = d$BBP_GIOPc = d$BBP_GSMc = d$OPTc <- NA

for(i in 1:nrow(d)){
  print(i)
  dd  <- d[i,]
  lat <- dd$LAT
  lon <- dd$LON
  mth <- dd$MONTH
  
  d$IMLDc[i]     <- mean(I[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$PARc[i]      <- mean(PAR[which.min((lon - lons)^2), which.min((lat - lats)^2),month(date)==mth]) 
  d$MLDc[i]      <- mean(MLD[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$KDc[i]       <- mean(KD[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$OPTDEPTHc[i] <- d$DEPTH[i]*d$KDc[i]
  d$PARc_z[i]    <- d$PARc[i]*exp(-d$KDc[i]*d$DEPTH[i])
}



##--GROUPING--############################
d$NGROUP   <- cut_number(d$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
d$CHLGROUP <- cut_number(d$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
d$ODGROUP  <- cut_number(d$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
d$PARGROUP <- cut_number(d$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups

##--NW ATLANTIC--##########################
#nwatl <- d[d$LAT > 35 & d$LAT < 70 & d$LON < -35 & d$LON >-80,]
nwatl <- d %>% filter(LAT>30 & LAT<80 & LON < -30 & LON > -90) 
nwatl <- nwatl[nwatl$NITRATE>-0.00001,]

nwatl$NGROUP   <- cut_number(nwatl$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$CHLGROUP <- cut_number(nwatl$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$ODGROUP  <- cut_number(nwatl$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$PARGROUP <- cut_number(nwatl$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups

nwatl$NGROUP   <- cut_number(nwatl$NITRATE,2,labels=c('1','2')) #create nitrate groups
nwatl$CHLGROUP <- cut_number(nwatl$TCHL,2,labels=c('1','2')) #create nitrate groups
nwatl$ODGROUP  <- cut_number(nwatl$OPTDEPTH,2,labels=c('1','2')) #create nitrate groups
nwatl$PARGROUP <- cut_number(nwatl$PARc_z,2,labels=c('1','2')) #create nitrate groups


write.csv(nwatl,file='processed_data/nwatl.csv')


# This section is intended to give nwatl PAR, MLD, kd, I at higher resolution
# still need to match up the lon/lat according to the lon/lat subset of nwatl

# nwatl$Imld_clim=nwatl$PAR_clim=nwatl$MLD_clim=nwatl$kd_clim <- NA
# 
# nlon <- 720*3
# nlat <- 360*30
# 
# lons <- seq(-180,180,length.out=nlon)
# lats <- seq(-90,90,length.out=nlat)
# 
# for(i in 1:nrow(nwatl)){
#   print(i)
#   dd  <- nwatl[i,]
#   lat <- dd$LAT
#   lon <- dd$LON
#   mth <- dd$MONTH
#   
#   nwatl$Imld_clim[i] <- mean(Iatl[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
#   nwatl$PAR_clim[i]  <- mean(PARatl[which.min((lon - lons)^2), which.min((lat - lats)^2),month(date)==mth]) 
#   nwatl$MLD_clim[i]  <- mean(MLDatl[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
#   nwatl$kd_clim[i]   <- mean(kdatl[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
# }

