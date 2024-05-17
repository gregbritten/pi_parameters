source('~/dropbox/code/functions/resize_bilinear().R')

library(fields)
###############################################
## CHL ########################################
###############################################
nc_chl <- nc_open("~/dropbox/working/pi_parameters/climatology/clim_chl.nc")

chl <- array(NA,dim=c(360,180,365))

lat <- ncvar_get(nc_chl,'lat')
lon <- ncvar_get(nc_chl,'lon')

for(i in 1:365){
print(i)
  g  <- ncvar_get(nc_chl,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,yout=360,xout=180) 
  chl[,,i] <- gg
}


image.plot(y=seq(-180,180,length.out=360),x=seq(-90,90,length.out=180),z=log10(gg))

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




