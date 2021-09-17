
d <- read_excel('~/google/WORKING/pi_parameters/data/PE_MAPP_2020_HAB_GKU_ADD-DATA_14062021.xlsx',col_types='numeric')

load(file='~/dropbox/working/pi_parameters/PARglb.rdata')	
load(file='~/dropbox/working/pi_parameters/MLDglb.rdata')	
load(file='~/dropbox/working/pi_parameters/kdglb.rdata')	
load(file='~/dropbox/working/pi_parameters/Iglb.rdata')	
load(file='~/dropbox/working/pi_parameters/PARatl.rdata')	
load(file='~/dropbox/working/pi_parameters/MLDatl.rdata')	
load(file='~/dropbox/working/pi_parameters/kdatl.rdata')	
load(file='~/dropbox/working/pi_parameters/Iatl.rdata')
load(file='~/dropbox/working/pi_parameters/date.rdata')

##--GLOBAL--############################
d$NGROUP   <- cut_number(d$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
d$CHLGROUP <- cut_number(d$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups

d$Imld_clim=d$PAR_clim=d$MLD_clim=d$kd_clim=d$OPTDEPTH=d$PAR_depth_clim <- NA

nlon <- 720
nlat <- 360

lons <- seq(-180,180,length.out=nlon)
lats <- seq(-90,90,length.out=nlat)

for(i in 1:nrow(d)){
print(i)
  dd  <- d[i,]
  lat <- dd$LAT
  lon <- dd$LON
  mth <- dd$MONTH
  
  d$Imld_clim[i] <- mean(Iglb[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$PAR_clim[i]  <- mean(PARglb[which.min((lon - lons)^2), which.min((lat - lats)^2),month(date)==mth]) 
  d$MLD_clim[i]  <- mean(MLDglb[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$kd_clim[i]   <- mean(kdglb[which.min((lon - lons)^2),   which.min((lat - lats)^2),month(date)==mth]) 
  d$OPTDEPTH[i]  <- d$DEPTH[i]*d$kd_clim[i]
  d$PAR_depth_clim[i] <- d$PAR_clim[i]*exp(-d$kd_clim[i]*d$DEPTH[i])
}

d$ODGROUP  <- cut_number(d$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
d$PARGROUP <- cut_number(d$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups


##--NW ATLANTIC--##########################
nwatl <- d[d$LAT > 35 & d$LAT < 70 & d$LON < -35 & d$LON >-80,]
nwatl <- nwatl[nwatl$NITRATE>-0.00001,]

nwatl$NGROUP   <- cut_number(nwatl$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$CHLGROUP <- cut_number(nwatl$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$ODGROUP  <- cut_number(nwatl$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
nwatl$PARGROUP <- cut_number(nwatl$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups


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

