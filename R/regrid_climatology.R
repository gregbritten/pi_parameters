source('~/dropbox/code/functions/resize_bilinear().R')

library(fields)
library(ncdf4)
###############################################
## CHL ########################################
###############################################
nc_chl <- nc_open("~/dropbox/working/pi_parameters/climatology/clim_chl.nc")
  lat <- ncvar_get(nc_chl,'lat')
  lon <- ncvar_get(nc_chl,'lon')

chl <- array(NA,dim=c(360,180,365))
for(i in 1:364){
print(i)
  g  <- ncvar_get(nc_chl,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,xout=360,yout=180)  
  chl[,,i] <- gg
}
nc_close(nc_chl)
saveRDS(chl,file='~/dropbox/working/pi_parameters/climatology/chl.rds')

###############################################
## PAR ########################################
###############################################
nc_par <- nc_open("~/dropbox/working/pi_parameters/climatology/clim_par.nc")

par <- array(NA,dim=c(360,180,365))
for(i in 1:365){
print(i)
  g  <- ncvar_get(nc_par,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,xout=360,yout=180) 
  par[,,i] <- gg
}
nc_close(nc_par)
saveRDS(par,file='~/dropbox/working/pi_parameters/climatology/par.rds')

###############################################
## SST ########################################
###############################################
nc_sst <- nc_open("~/dropbox/working/pi_parameters/climatology/clim_sst.nc")

sst <- array(NA,dim=c(360,180,365))
for(i in 1:365){
  print(i)
  g  <- ncvar_get(nc_sst,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,xout=360,yout=180) 
  sst[,,i] <- gg
}
nc_close(nc_sst)
saveRDS(sst,file='~/dropbox/working/pi_parameters/climatology/sst.rds')





