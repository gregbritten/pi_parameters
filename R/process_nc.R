
library(ncdf4)

aprx <- function(Y){
  lapply(1:ncol(Y),function(p){
    approx(x=1:366,y=Y[,p],xout=1:366,rule=2)$y
  })
}

nc <- nc_open('~/dropbox/working/pi_parameters/mapps_forest/upstream_files/mapps_upstream_sat_365.nc')

sstnc <- ncvar_get(nc,'sst')
parnc <- ncvar_get(nc,'par')
chlnc <- ncvar_get(nc,'chl')

ncrit <- 1
filt  <- c(which(apply(sstnc,2,function(x) sum(complete.cases(x)))<ncrit),
           which(apply(parnc,2,function(x) sum(complete.cases(x)))<ncrit),
           which(apply(chlnc,2,function(x) sum(complete.cases(x)))<ncrit))
keep  <- {1:ncol(sstnc)}[-filt]
n     <- length(keep)

sst <- matrix(unlist(aprx(sstnc[,keep])),ncol=n)
par <- matrix(unlist(aprx(parnc[,keep])),ncol=n)
chl <- matrix(unlist(aprx(chlnc[,keep])),ncol=n)

lat   <- ncvar_get(nc, 'lat')[keep]
lon   <- ncvar_get(nc, 'lon')[keep]
depth <- ncvar_get(nc, 'depth')[keep] 
month <- ncvar_get(nc, 'month')[keep] 
daylength <- ncvar_get(nc, 'daylength')[keep]

PBmax <- ncvar_get(nc, 'PBmax')[keep]
alpha <- ncvar_get(nc, 'alpha')[keep]
Ek    <- ncvar_get(nc, 'Ek')[keep]

region = ifelse(lat> -40 & lat<0 & lon> -150 & lon< -70,'spac',
                ifelse(lat> -60 & lat< -40 & lon< 160 & lon>130,'tas',
                       ifelse(lat> 30 & lat<50 & lon> -70 & lon< -40,'scot',
                              ifelse(lat>= 50 & lat<65 & lon> -60 & lon< -40,'lab',
                                     ifelse(lat>= 58 & lat<72 & lon> -35 & lon< 0,'ice',NA)))))


##################################################
sst2 <- sstnc[,keep]
par2 <- parnc[,keep]
chl2 <- chlnc[,keep]

D <- list()
D[[1]] <- data.frame(sst=sst2[1,],chl=chl2[1,],par=par2[1,],
                     lat=lat,lon=lon,depth=depth,
                     month=month,daylength=daylength,
                     PBmax=PBmax,alpha=alpha,Ek=Ek,
                     region=region)
for(i in 2:365){
  D[[i]] <- data.frame(sst=colMeans(sst2[1:i,],na.rm=TRUE),
                       chl=colMeans(chl2[1:i,],na.rm=TRUE),
                       par=colMeans(par2[1:i,],na.rm=TRUE),
                       lat=lat,lon=lon,depth=depth,
                       month=month,daylength=daylength,
                       PBmax=PBmax,alpha=alpha,Ek=Ek,
                       region=region)
}








