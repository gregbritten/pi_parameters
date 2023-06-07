#source('r/process_csvs.r') ##--data processing--###########

i=1
par(mfrow=c(2,3))
for(j in 1:5){
  d     <- D[[i]] %>% filter(region==regions[j],complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
  rr <- d$Ez/d$Ek
  hist(rr[rr<5],xlim=c(0,10),breaks=seq(0,10,0.5),main='')
  mtext(region_long[j])
}
d     <- D[[i]] %>% filter(region%in%regions,complete.cases(chl,kd_490,sst,par,lat,lon,depth,month,daylength))
rr <- d$Ek/d$Ez
hist(rr[rr<5],xlim=c(0,10),breaks=seq(0,10,0.5),main='')
mtext('Global')

