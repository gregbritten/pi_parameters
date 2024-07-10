library(fields)
library(ncdf4)

##--function for interpolation
resize_bilinear <- function(xin,yin,xout,yout,z){
  obj   <- list(x=1:xin, y=1:yin, z = z)
  tempx <- seq(1,xin,length.out=xout)
  tempy <- seq(1,yin,length.out=yout)
  loc   <- make.surface.grid(list(tempx,tempy))
  look  <- interp.surface(obj,loc)
  return(as.surface(loc,look)$z)
}

##--chl--#################
nc_chl <- nc_open("processed_data/clim_chl.nc")
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
saveRDS(chl,file='processed_data/chl.rds')

##--par--####################
nc_par <- nc_open("processed_data/clim_par.nc")

par <- array(NA,dim=c(360,180,365))
for(i in 1:365){
print(i)
  g  <- ncvar_get(nc_par,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,xout=360,yout=180) 
  par[,,i] <- gg
}
nc_close(nc_par)
saveRDS(par,file='processed_data/par.rds')

##--sst--######################
nc_sst <- nc_open("processed_data/clim_sst.nc")

sst <- array(NA,dim=c(360,180,365))
for(i in 1:365){
  print(i)
  g  <- ncvar_get(nc_sst,start=c(1,1,i),count=c(-1,-1,1))
  gg <- resize_bilinear(z=g,xin=8640,yin=4320,xout=360,yout=180) 
  sst[,,i] <- gg
}
nc_close(nc_sst)
saveRDS(sst,file='processed_data/sst.rds')


