rm(list=ls())

library(readxl)
library(rgdal)
library(maptools)
library(maps)
library(raster)
library(tidyverse)

##--PROCESS DATA--################
source('~/dropbox/working/pi_parameters/github/data_process.R')

d <- d[d$ALPHA<1 & d$PMB<10 & d$EK<700 & d$OPTDEPTH<10 & d$NITRATE>0,]

dat <- d[complete.cases(d$TEMP),]

dat$NGROUP   <- cut_number(dat$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
dat$CHLGROUP <- cut_number(dat$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
dat$ODGROUP  <- cut_number(dat$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
dat$PARGROUP <- cut_number(dat$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups

##--MAP--##########

pdf('~/google/working/pi_parameters/plots/global_map.pdf',height=7,width=7)
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
maps::map(fill=TRUE,col='grey',ylim=c(-90,90),xlim=c(-180,180))
points(d$LON,d$LAT,cex=0.5,col='red')
axis(side=1)
axis(side=2,at=c(-90,-45,0,45,90))
# axis(side=1,at=c(-180,-90,0,90,180))
# axis(side=2,at=c(-90,-45,0,45,90))
#mtext('d Transect')
dev.off()

#######################################################
##--GLOBAL--###########################################
#######################################################
##--MAP--##########
pdf('~/google/working/pi_parameters/plots/covariates_n.pdf',height=5,width=9)
par(mfrow=c(2,2),mar=c(0,0,0,0),oma=c(0,0,0,0))
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(d$LON,d$LAT,cex=0.2,col='red')
mtext(bquote('All Points  '~italic('n')~'='~.(nrow(d))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- d[complete.cases(d$TEMP),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature  '~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- dd[complete.cases(dd$NITRATE),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature & Nitrate'~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dd <- dd[complete.cases(dd$OPTDEPTH),]
maps::map(fill=TRUE,col='grey',xlim=c(-180,180),ylim=c(-90,90))
points(dd$LON,dd$LAT,cex=0.2,col='red')
mtext(bquote('With Temperature & Nitrate & Optical Depth'~italic('n')~'='~.(nrow(dd))))
axis(side=1,at=c(-180,-90,0,90,180)); axis(side=2,at=c(-90,-45,0,45,90))

dev.off()


##--ALPHA, PMB, EK vs. LATITUDE--##########
pdf('~/google/working/pi_parameters/plots/d_parameters_lat_depth.pdf',height=6,width=8)
order = findInterval(d$DEPTH, sort(d$DEPTH))
par(mfrow=c(3,1),mar=c(2,2,2,8),oma=c(3,3,3,3))
plot(d$LAT,d$PMB,col=viridis(nrow(d))[order],pch=19,xlim=c(-55,55))
mtext(side=2,bquote('P'['m']^'B'),line=2.5)
image.plot(legend.only=TRUE, matrix(d$DEPTH),col=viridis(20))
plot(d$LAT,d$ALPHA,col=viridis(nrow(d))[order],pch=19,xlim=c(-55,55))
mtext(side=2,bquote(alpha^'B'),line=2.5)
plot(d$LAT,d$EK,col=viridis(nrow(d))[order],pch=19,xlim=c(-55,55))
mtext(side=2,bquote('E'['k']),line=2.5)
mtext('Latitude',side=1,line=2.5)
dev.off()

d$NGROUP   <- cut_number(d$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
dat$NGROUP   <- cut_number(dat$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups

d$CHLGROUP <- cut_number(d$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
dat$CHLGROUP <- cut_number(dat$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups



pdf('~/google/working/pi_parameters/plots/pmb_alpha_ek_scatter_global.pdf',height=6,width=10)
samp <- sample(1:nrow(d),size=2000)
par(mfcol=c(3,5),mar=c(2,2,1,1),oma=c(2,3,2,2))
plot(d$TEMP[samp],d$PMB[samp],cex=0.3,xlim=c(-3,35),ylim=c(0,20))
mtext(expression('P'['m']^{'B'}),side=2,line=2.5)
plot(d$TEMP[samp],d$ALPHA[samp],xlim=c(-3,35),cex=0.3,ylim=c(0,0.2))
mtext(expression(alpha^{'B'}),side=2,line=2.5)
plot(d$TEMP[samp],d$EK[samp],cex=0.3,xlim=c(-3,35),ylim=c(0,500))
mtext(expression('E'['k']),side=2,line=2.5)
mtext('Temperature',side=1,line=2.5)

# plot(d$NITRATE,d$PMB,cex=0.3,xlim=c(0,1),ylim=c(0,15))
# plot(d$NITRATE,d$ALPHA,xlim=c(0,1),cex=0.3,ylim=c(0,0.2))
# plot(d$NITRATE,d$EK,cex=0.3,xlim=c(0,1))
# mtext('Nitrate',side=1,line=2.5)

plot(log10(d$NITRATE[samp]+0.1),d$PMB[samp],cex=0.3,ylim=c(0,20),xlim=c(-1.2,2))
plot(log10(d$NITRATE[samp]+0.1),d$ALPHA[samp],cex=0.3,ylim=c(0,0.2),xlim=c(-1.2,2))
plot(log10(d$NITRATE[samp]+0.1),d$EK[samp],cex=0.3,ylim=c(0,500),xlim=c(-1.2,2))
mtext('log10 Nitrate',side=1,line=2.5)

# plot(d$TCHL,d$PMB,cex=0.3,xlim=c(0,2),ylim=c(0,15))
# plot(d$TCHL,d$ALPHA,xlim=c(0,2),cex=0.3,ylim=c(0,0.2))
# plot(d$TCHL,d$EK,cex=0.3,xlim=c(0,2))
# mtext('Chlorophyll',side=1,line=2.5)

plot(log10(d$TCHL[samp]+0.1),d$PMB[samp],cex=0.3,ylim=c(0,20),xlim=c(-1.2,2))
plot(log10(d$TCHL[samp]+0.1),d$ALPHA[samp],cex=0.3,ylim=c(0,0.2),xlim=c(-1.2,2))
plot(log10(d$TCHL[samp]+0.1),d$EK[samp],cex=0.3,xlim=c(-1.2,2))
mtext('log10 Chlorophyll',side=1,line=2.5)

# plot(d$Imld_clim,d$PMB,cex=0.3,xlim=c(0,20),ylim=c(0,15))
# plot(d$Imld_clim,d$ALPHA,xlim=c(0,20),cex=0.3,ylim=c(0,0.2))
# plot(d$Imld_clim,d$EK,cex=0.3)
# mtext('Chlorophyll',side=1,line=2.5)

plot(d$DEPTH[samp],d$PMB[samp],cex=0.3,ylim=c(0,20),xlim=c(0,120))
plot(d$DEPTH[samp],d$ALPHA[samp],cex=0.3,ylim=c(0,0.2),xlim=c(0,120))
plot(d$DEPTH[samp],d$EK[samp],cex=0.3,xlim=c(0,120))
mtext('Depth',side=1,line=2.5)

plot(d$DEPTH[samp]*d$kd_clim[samp],d$PMB[samp],cex=0.3,ylim=c(0,20),xlim=c(0,8))
plot(d$DEPTH*d$kd_clim,d$ALPHA,cex=0.3,ylim=c(0,0.2),xlim=c(0,8))
plot(d$DEPTH*d$kd_clim,d$EK,cex=0.3,xlim=c(-0.5,9))
mtext('Optical Depth',side=1,line=2.5)

# plot(d$PAR_clim,d$PMB,cex=0.3,ylim=c(0,15))
# plot(d$PAR_clim,d$ALPHA,cex=0.3,ylim=c(0,0.2))
# plot(d$PAR_clim,d$EK,cex=0.3)
# mtext('Chlorophyll',side=1,line=2.5)
# # 
# plot(d$PAR_clim*exp(-d$kd_clim*d$DEPTH),d$PMB,cex=0.3,ylim=c(0,15))
# plot(d$PAR_clim*exp(-d$kd_clim*d$DEPTH),d$ALPHA,cex=0.3,ylim=c(0,0.2))
# plot(d$PAR_clim*exp(-d$kd_clim*d$DEPTH),d$EK,cex=0.3)
# mtext('Irradiance at Depth',side=1,line=2.5)


dev.off()


pdf('~/google/working/pi_parameters/plots/env_scatter_atl.pdf',height=6,width=7/2)
par(mfcol=c(3,1),mar=c(2,2,2,2),oma=c(2,3,2,2))
plot(d$NITRATE,d$TEMP,ylim=c(-2,30),xlim=c(0,20),pch=19,cex=0.5)
mtext(side=1,'Nitrate',line=2.5)
mtext(side=2,'Temperature',line=2.5)
plot(d$NITRATE,d$TCHL,ylim=c(0,30),xlim=c(0,20),pch=19,cex=0.5)
mtext(side=1,'Nitrate',line=2.5)
mtext(side=2,'Chlorophyll',line=2.5)
plot(d$TEMP,d$TCHL,ylim=c(0,30),xlim=c(-2,30),pch=19,cex=0.5)
mtext(side=1,'Temperature',line=2.5)
mtext(side=2,'Chlorophyll',line=2.5)
dev.off()


###########################################################################
##--TEMPERATURE DEPENDENCE OF PBM--########################################
###########################################################################
unique(cut_number(dat$NITRATE,4)) #create nitrate groups
unique(cut_number(dat$TCHL,4)) #create nitrate groups
unique(cut_number(dat$OPTDEPTH,4)) #create nitrate groups
unique(cut_number(dat$PAR_depth_clim,4)) #create nitrate groups


NGROUPS   <- c(0.01,0.11,1.36,7.5,34.5)
CHLGROUPS <- c(0.01,0.2,0.46,1.3,105)
ODGROUPS  <- c(0.01,0.734,1.75,2.99,89.9)
PARGROUPS <- c(0.01,1.79,5.85,16.6,57.5)

#dat$NGROUP   <- cut_number(dat$NITRATE,3,labels=c('1','2','3')) #create nitrate groups
#dat$CHLGROUP <- cut_number(dat$TCHL,3,labels=c('1','2','3')) #create nitrate groups


tT <- function(TEMP, a, b, z, w) a*exp(b*TEMP)*(1-((TEMP-z)/w)^2)
tT2 <- function(TEMP, z, w) 2.5*exp(0.0631*TEMP)*(1-((TEMP-z)/w)^2)
tT3 <- function(TEMP, a, z, w) a*exp(0.0631*TEMP)*(1-((TEMP-z)/w)^2)

tT_behren <- function(TEMP) -(3.27E-8)*TEMP^7 + (3.4132E-6)*TEMP^6 - (1.348E-4)*TEMP^5 + (2.462E-3)*TEMP^4 - (0.0205)*TEMP^3 + 0.0617*TEMP^2 + 0.2749*TEMP + 1.2956  

tT_opt <- function(a,b,z,w) (b*z + sqrt(b^2 * w^2 + 1) - 1)/b

tT_max <- function(a,b,z,w) 2*a*exp(b*z + sqrt(b^2*w^2 + 1) - 1)*(sqrt(b^2*w^2 + 1) - 1)/(b^2*w^2)
####################################################################
## TEMPERATURE AND CHLOROPHYLL GROUPS ##############################
####################################################################
fits <- fitgroup <- function(dat,group){
  if(group=='NGROUP'){dat1 <- dat[dat$NGROUP==1,]; dat2   <- dat[dat$NGROUP==2,]; dat3   <- dat[dat$NGROUP==3,];   dat4 <- dat[dat$NGROUP==4,]}
  if(group=='CHLGROUP'){dat1 <- dat[dat$CHLGROUP==1,]; dat2 <- dat[dat$CHLGROUP==2,]; dat3 <- dat[dat$CHLGROUP==3,]; dat4 <- dat[dat$CHLGROUP==4,]}
  if(group=='ODGROUP'){dat1 <- dat[dat$ODGROUP==1,]; dat2 <- dat[dat$ODGROUP==2,]; dat3 <- dat[dat$ODGROUP==3,]; dat4 <- dat[dat$ODGROUP==4,]}
  if(group=='PARGROUP'){dat1 <- dat[dat$PARGROUP==1,]; dat2 <- dat[dat$PARGROUP==2,]; dat3 <- dat[dat$PARGROUP==3,]; dat4 <- dat[dat$PARGROUP==4,]}
  
  fit1 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat1, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  fit2 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat2, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  fit3 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat3, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  fit4 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat4, start=list(a=2.5,b=0.06,z=8,w=14),control=list(warnOnly=TRUE))
  
  lines(seq(0,30,0.01),predict(fit1,newdata=list(TEMP=seq(0,30,0.01))),col=1)  
  lines(seq(0,30,0.01),predict(fit2,newdata=list(TEMP=seq(0,30,0.01))),col=2)  
  lines(seq(0,30,0.01),predict(fit3,newdata=list(TEMP=seq(0,30,0.01))),col=3)  
  lines(seq(0,30,0.01),predict(fit4,newdata=list(TEMP=seq(0,30,0.01))),col=4) 
  
  fits <- list(fit1,fit2,fit3,fit4)
  return(fits)
}
#dat1 <- dat[dat$NGROUP==1,]; dat2 <- dat[dat$NGROUP==2,]; dat3 <- dat[dat$NGROUP==3,]; 

pdf('~/google/working/pi_parameters/plots/PMB_temperature_groups_global.pdf',height=6,width=12)
par(mfrow=c(2,3),mar=c(2,2,2,4),oma=c(2,3,2,2))
samp <- sample(1:nrow(d),size=3000)

plot(dat$TEMP[samp], dat$PMB[samp],cex=0.5,xlim=c(-5,32),ylim=c(0,20))
#fit  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat, start=list(a=2.5,b=0.06,z=13,w=14))
#fit  <- nls(PMB ~ tT(TEMP,a,bz,w), data=dat, start=list(z=13,w=14))
#lines(seq(0,30,0.01),predict(fit,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  
#legend(legend=c('Global Eppley-like', 'Behrenfeld & Falkowski'),'topleft',lty=1,col=c('red','blue'),bty='n')

plot(dat$TEMP[samp], dat$PMB[samp], col=dat$NGROUP[samp],pch=1,cex=0.5,xlim=c(-5,32),ylim=c(0,20))
image.plot(legend.only=TRUE,matrix(log10(dat$NITRATE)),col=1:4,breaks=log10(NGROUPS))
mtext('Grouped by nitrate')
#FITS_NGROUP <- fits(dat,'NGROUP')

plot(dat$TEMP[samp], dat$PMB[samp], col=dat$CHLGROUP[samp],cex=0.5,xlim=c(-5,32),ylim=c(0,20))
image.plot(legend.only=TRUE,matrix(log10(dat$TCHL)),col=1:4,breaks=log10(CHLGROUPS))
mtext('Grouped by chlorophyll')
#FITS_CHLGROUP <- fits(dat,'CHLGROUP')

plot(dat$TEMP[samp], dat$PMB[samp], col=dat$ODGROUP[samp],cex=0.5,xlim=c(-5,32))
image.plot(legend.only=TRUE,matrix(log10(dat$OPTDEPTH)),col=1:4,breaks=log10(ODGROUPS))
mtext('Grouped by optical depth')
#FITS_ODGROUP <- fits(dat,'ODGROUP')

plot(dat$TEMP, dat$PMB, col=dat$PARGROUP,cex=0.5,xlim=c(-5,32))
image.plot(legend.only=TRUE,matrix(log10(dat$PAR_depth_clim)),col=1:4,breaks=log10(PARGROUPS))
mtext('Grouped by PAR')
#FITS_PARGROUP <- fits(dat,'PARGROUP')

mtext(side=2,outer=TRUE,expression('P'['m']^{'B'}),line=0.5)
mtext(outer=TRUE,expression('Groups determined by equal size binning using log10 transform'),line=0.5)

dev.off()


makedf <- function(FITS){
  PARS <- list()
  for(i in 1:length(FITS)){
    PARS[[i]] <- summary(FITS[[i]])$coefficients[,1]
  }
  
  df <- data.frame(a=PARS[[1]]['a'], 
                   b=PARS[[1]]['b'], 
                   Topt=   tT_opt(PARS[[1]]['a'], PARS[[1]]['b'], PARS[[1]]['z'], PARS[[1]]['w']),
                   PBMmax=tT_max(PARS[[1]]['a'], PARS[[1]]['b'], PARS[[1]]['z'], PARS[[1]]['w']))
  for(i in 2:length(FITS)){
    df_tmp <- data.frame(a=PARS[[i]]['a'],
                         b=PARS[[i]]['b'],
                         Topt=tT_opt(PARS[[i]]['a'], PARS[[i]]['b'], PARS[[i]]['z'], PARS[[i]]['w']),
                         PBMmax=tT_max(PARS[[i]]['a'], PARS[[i]]['b'], PARS[[i]]['z'], PARS[[i]]['w']))
    df <- rbind(df,df_tmp)
  }
  return(df)
}

makedf(FITS_CHLGROUP)
makedf(FITS_NGROUP)


############################################################################################
## MULTIPLE LINEAR REGRESSION FITS #########################################################
############################################################################################
fit <- lm(PMB ~ TEMP + NITRATE + TCHL + OPTDEPTH, data=d)
fit <- lm(PMB ~ TEMP + log10(NITRATE+0.1) + log10(TCHL) + OPTDEPTH, data=d)

pdf('~/google/working/pi_parameters/plots/lm_fits_global.pdf',height=6,width=6)
avPlots(fit,id=FALSE)
dev.off()

pdf('~/google/working/pi_parameters/plots/lm_fits_global_cor.pdf',height=4,width=4)
plot(d$PMB, predict(fit, newdata=d),xlim=c(0,10),ylim=c(0,10),cex=0.5,xlab='',ylab='')
mtext(side=1,line=2.5, 'Observed'); mtext(side=2,line=2.5, 'Predicted') 
mtext(bquote('cor = '*.(round(cor(d$PMB,predict(fit,newdata=d),use='pairwise.complete.obs'),3))),line=-1.5)
abline(0,1) 
dev.off()

#####################################################################
## GAM FITS #########################################################
#####################################################################
pdf('~/google/working/pi_parameters/plots/gam_fits_global.pdf',height=5,width=11)
sp <- 1
par(mfrow=c(2,5),mar=c(2,2,2,2),oma=c(2,2,2,2))
datt <- data.frame(PMB=dat$PMB, NITRATE=dat$NITRATE, TEMP=dat$TEMP, TCHL=dat$TCHL, OPTDEPTH=dat$OPTDEPTH)
datt <- datt[complete.cases(datt),]
samp <- sample(1:nrow(datt),size=nrow(datt)/2)
train <- datt[samp,]
test  <- datt[-samp,]

fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPTDEPTH,sp=sp), data=train)
fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPTDEPTH,sp=sp), data=datt)
plot.gam(fit,select=1, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('Temperature',side=1,line=2.5) 
plot.gam(fit,select=2, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('Nitrate',side=1,line=2.5)
plot.gam(fit,select=3, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('Chlorophyll',side=1,line=2.5)
plot.gam(fit,select=4, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('Optical depth',side=1,line=2.5)
plot(datt$PMB,predict(fit,newdata=datt),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
mtext(bquote('cor = '*.(round(cor(dat$PMB,predict(fit,newdata=dat),use='pairwise.complete.obs'),3))),line=-1.5)
abline(0,1)

#datt <- data.frame(PMB=dat$PMB, NITRATE=log10(dat$NITRATE+0.1), TEMP=dat$TEMP, TCHL=log10(dat$TCHL), OPDEPTH=log10(dat$OPTDEPTH+0.1))
datt <- data.frame(PMB=dat$PMB, NITRATE=log10(dat$NITRATE+0.1), TEMP=dat$TEMP, TCHL=log10(dat$TCHL), OPDEPTH=log10(dat$OPTDEPTH+0.1))
datt <- datt[complete.cases(datt),]
samp <- sample(1:nrow(datt),size=nrow(datt)/2)
train <- datt[samp,]
test  <- datt[-samp,]

fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPDEPTH,sp=sp), data=train)
fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPDEPTH,sp=sp), data=datt)
plot.gam(fit,select=1, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('Temperature',side=1,line=2.5) 
plot.gam(fit,select=2, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('log Nitrate',side=1,line=2.5)
plot.gam(fit,select=3, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('log Chlorophyll',side=1,line=2.5)
plot.gam(fit,select=4, residuals=TRUE,lwd=3); mtext('Effect Size',side=2,line=2.5); mtext('log Optical depth',side=1,line=2.5)
#plot(test$PMB,predict(fit,newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
plot(datt$PMB,predict(fit,newdata=datt),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))),line=-1.5)
abline(0,1)
dev.off()

# fit <- gam(log10(PMB) ~ s(TEMP,sp=sp) + s(log10(NITRATE+0.1),sp=sp) + s(log10(TCHL),sp=sp), data=dat)
# plot.gam(fit,select=1, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Temperature',side=1,line=2.5)
# plot.gam(fit,select=2, residuals=TRUE); mtext('Nitrate',side=1,line=2.5)
# plot.gam(fit,select=3, residuals=TRUE); mtext()
# plot(dat$PMB,10^(predict(fit,newdata=dat)),xlim=c(0,10),ylim=c(0,10),cex=0.5)
# mtext(bquote('cor = '*.(round(cor(dat$PMB,predict(fit,newdata=dat),use='pairwise.complete.obs'),3))))
# abline(0,1)

###################################################################
## NEURAL NETWORK #################################################
###################################################################
# par(mfrow=c(1,3))
# datt <- data.frame(PMB=dat$PMB, NITRATE=dat$NITRATE, TEMP=dat$TEMP, TCHL=dat$TCHL)
# datt <- datt[complete.cases(datt),]
# nfit <- neuralnet(PMB ~ TEMP + TCHL + NITRATE, data=datt, hidden=10, stepmax=1E8, startweights=0)
# plot(datt$PMB, predict(nfit,newdata=datt))
# mtext(bquote('cor = '*.(round(cor(dat$PMB,predict(nfit,newdata=dat),use='pairwise.complete.obs'),3))))
# 
# datt <- data.frame(PMB=dat$PMB, NITRATE=log10(dat$NITRATE+0.1), TEMP=dat$TEMP, TCHL=log10(dat$TCHL))
# datt <- datt[complete.cases(datt),]
# nfit <- neuralnet(PMB ~ TEMP + TCHL + NITRATE, data=datt, hidden=10, stepmax=1E8, startweights=0)
# plot(datt$PMB, predict(nfit,newdata=datt))
# mtext(bquote('cor = '*.(round(cor(dat$PMB,predict(nfit,newdata=dat),use='pairwise.complete.obs'),3))))


###################################################################
## RANDOM FOREST ##################################################
###################################################################
datt <- data.frame(PMB=dat$PMB, NITRATE=log10(dat$NITRATE+0.1), TEMP=dat$TEMP, TCHL=log10(dat$TCHL), OPTDEPTH=log10(dat$OPTDEPTH+0.1), PAR=log10(dat$PAR_clim))
#datt <- data.frame(PMB=dat$PMB, NITRATE=dat$NITRATE, TEMP=dat$TEMP, TCHL=dat$TCHL)

datt <- datt[complete.cases(datt),]
samp <- sample(1:nrow(datt),size=nrow(datt)/2)
train <- datt[samp,]
test  <- datt[-samp,]

pdf('~/google/working/pi_parameters/plots/rf_fits_global.pdf',height=8,width=6)
par(mfrow=c(4,2),mar=c(2,2,3,2),oma=c(2,2,2,2))
fit <- randomForest(PMB ~ TEMP, data=train)
plot(train$PMB, predict(fit, newdata=train),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext('Temperature')
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)
plot(test$PMB, predict(fit, newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))))
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)

fit <- randomForest(PMB ~ TEMP + TCHL , data=train)
plot(train$PMB, predict(fit, newdata=train),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext('Temperature + Chlorophyll')
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)
plot(test$PMB, predict(fit, newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))))
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)

fit <- randomForest(PMB ~ TEMP + TCHL + NITRATE, data=train)
plot(train$PMB, predict(fit, newdata=train),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext('Temperature + Chlorophyll + Nitrate')
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)
plot(test$PMB, predict(fit, newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))))
abline(0,1)

fit <- randomForest(PMB ~ TEMP + TCHL + NITRATE + OPTDEPTH, data=train)
plot(train$PMB, predict(fit, newdata=train),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext('Temperature + Chlorophyll + Nitrate + Opt Depth',cex=0.7)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
abline(0,1)
plot(test$PMB, predict(fit, newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))))
abline(0,1)

# fit <- randomForest(PMB ~ TEMP + TCHL + NITRATE + OPTDEPTH + PAR, data=train)
# plot(train$PMB, predict(fit, newdata=train),xlim=c(0,10),ylim=c(0,10),cex=0.5)
# mtext('Temperature + Chlorophyll + Nitrate')
# mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
# abline(0,1)
# plot(test$PMB, predict(fit, newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
# mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
# mtext(bquote('cor = '*.(round(cor(test$PMB,predict(fit,newdata=test),use='pairwise.complete.obs'),3))))
# abline(0,1)

dev.off()


################################################################################
## TESTING #####################################################################
################################################################################

##--VISUALIZING THE RANDOM FOREST--########################
fit <- cforest(PMB ~ TEMP + NITRATE + TCHL + OPTDEPTH, data=train)
pt <- prettytree(fit@ensemble[[2]], names(fit@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- fit@data 
nt@responses <- fit@responses 

plot(nt, type="simple",cex=0.6)


##--FIT RANDOM EFFECTS MODEL--##############################
fit_re <- nlme(PMB ~ tT(TEMP,a,b,z,w), 
               data=dat, 
               fixed  = a + b + z + w ~ 1,
               random = z ~ 1, 
               groups = ~ NGROUP,
               start=c(a=2.5,b=0.06,z=10,w=14),
               control=list(msMaxIter=500, minScale=1e-4))

fit_re <- nlme(PMB ~ tT2(TEMP,z,w), 
               data=dat, 
               fixed  = z + w ~ 1,
               random = z + w ~ 1, 
               groups = ~ NGROUP,
               start=c(z=10,w=14),
               control=list(msMaxIter=500, minScale=1e-4))



pars   <- fit_re$coefficients$fixed
res_a  <- fit_re$coefficients$random$NGROUP[,1]
res_b  <- fit_re$coefficients$random$NGROUP[,2]
res_w  <- fit_re$coefficients$random$NGROUP[,1]

order = findInterval(dat$NITRATE, sort(dat$NITRATE))
dat$col <- viridis(nrow(dat))[order]

par(mfrow=c(1,1))
plot(dat$TEMP,dat$PMB,col=dat$col)
lines(Tin, tT(Tin,pars[1],pars[2],pars[3],pars[4])) #first guess parameters
lines(Tin, tT(Tin,pars[1]+res_a[1],pars[2]+res_b[1],pars[3],pars[4])) #first guess parameters
lines(Tin, tT(Tin,pars[1]+res[2],pars[2]+res_b[2],pars[3],pars[4])) #first guess parameters
lines(Tin, tT(Tin,pars[1]+res[3],pars[2]+res_b[3],pars[3],pars[4])) #first guess parameters

lines(Tin, tT(Tin,pars[1],pars[2],pars[3],pars[4]+res_w[1])) #first guess parameters
lines(Tin, tT(Tin,pars[1],pars[2],pars[3],pars[4]+res_w[3])) #first guess parameters



order = findInterval(d$DEPTH, sort(d$DEPTH))
d$col <- viridis(nrow(d))[order]

pdf('~/google/working/pi_parameters/plots/d_parameters_depth_lat.pdf',height=7,width=7)
par(mfrow=c(3,1),mar=c(2,2,2,8),oma=c(3,3,3,3))
#plot(d$LAT,d$PMB,col=viridis(nrow(d))[order],pch=19,xlim=c(35,70),cex=0.5)
plot(d$LAT,d$PMB,col=d$col,pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('P'['m']^'B'),line=2.5)
image.plot(legend.only=TRUE, matrix(d$DEPTH),col=viridis(20))
plot(d$LAT,d$ALPHA,col=viridis(nrow(d))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote(alpha^'B'),line=2.5)
plot(d$LAT,d$EK,col=viridis(nrow(d))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('E'['k']),line=2.5)
mtext('Latitude',side=1,line=2.5)
dev.off()


