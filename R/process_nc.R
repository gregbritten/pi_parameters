
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

ncrit <- 30
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
PBmax <- ncvar_get(nc, 'PBmax')[keep]
alpha <- ncvar_get(nc, 'alpha')[keep]
Ek    <- ncvar_get(nc, 'Ek')[keep]



