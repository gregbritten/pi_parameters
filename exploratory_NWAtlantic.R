library(randomForest)
library(car)

rm(list=ls())

##--PROCESS DATA--################
source('~/dropbox/working/pi_parameters/github/data_process.R')

plot(nwatl$DEPTH*nwatl$kd_clim,log10(nwatl$EK),col=nwatl$NGROUP)
plot(nwatl$DEPTH*nwatl$kd_clim,log10(nwatl$EK),col=nwatl$CHLGROUP)


pdf('~/google/working/pi_parameters/plots/north_atlantic_points.pdf',height=6,width=5)
par(mfrow=c(1,1))
maps::map(fill=TRUE,col='grey',xlim=c(-90,-20),ylim=c(25,80))
points(nwatl$LON,nwatl$LAT,cex=0.2,col='red')
axis(side=1); axis(side=2)
dev.off()


##--seasonal decomposition--###############
seas <- list()
seas[['winter']] <- c(12,1,2)
seas[['spring']] <- c(3,4,5)
seas[['summer']] <- c(6,7,8)
seas[['fall']]   <- c(9,10,11)


##--PLOT PARAMETERS VS LATITUDE BY SEASON--###########################
order     <- findInterval(log10(nwatl$DEPTH), sort(log10(nwatl$DEPTH)))
nwatl$col <- viridis(nrow(nwatl))[order]

pdf('~/google/working/pi_parameters/plots/NWAtl_parameters_depth_lat_season.pdf',height=7,width=11)
par(mfcol=c(3,4),mar=c(2,3,2,2),oma=c(3,3,3,3))
for(i in 1:4){
  nwatl_tmp <- nwatl[nwatl$MONTH %in% seas[[i]],]
  #order     <- findInterval(c(nwatl_tmp$DEPTH,max(nwatl$DEPTH,na.rm=TRUE)), sort(c(nwatl_tmp$DEPTH,max(nwatl$DEPTH,na.rm=TRUE))))
  #nwatl_tmp$col <- viridis(nrow(nwatl_tmp))[order]
  #plot(nwatl_tmp$LAT,nwatl_tmp$PMB,col=viridis(nrow(nwatl_tmp))[order],pch=19,xlim=c(35,70),ylim=c(0,20))
  plot(nwatl_tmp$LAT,nwatl_tmp$PMB,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,20),cex=0.5)
  mtext(side=2,bquote(italic('P'['m']^'B')),line=2.5)
  mtext(names(seas[i]))
  plot(nwatl_tmp$LAT,nwatl_tmp$ALPHA,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,0.4),cex=0.5)
  mtext(side=2,bquote(alpha^'B'),line=2.5)
  plot(nwatl_tmp$LAT,nwatl_tmp$EK,col=nwatl_tmp$col,pch=19,xlim=c(35,70),ylim=c(0,700),cex=0.5)
  mtext(side=2,bquote('E'['k']),line=2.5)
  mtext('Latitude',side=1,line=2.5)
}
image.plot(legend.only=TRUE, add=TRUE,matrix(nwatl$DEPTH),col=viridis(20))
dev.off()


##--PLOT BIVARIATE RELATIONSHIPS--######################################

pdf('~/google/working/pi_parameters/plots/pmb_alpha_ek_scatter_atl.pdf',height=6,width=10)
par(mfcol=c(3,5),mar=c(2,2,1,1),oma=c(2,3,2,2))
plot(nwatl$TEMP,nwatl$PMB,cex=0.3,xlim=c(-2,30),ylim=c(0,15))
mtext(expression('P'['m']^{'B'}),side=2,line=2.5)
plot(nwatl$TEMP,nwatl$ALPHA,xlim=c(-2,30),cex=0.3,ylim=c(0,0.2))
mtext(expression(alpha^{'B'}),side=2,line=2.5)
plot(nwatl$TEMP,nwatl$EK,cex=0.3,xlim=c(-2,30))
mtext(expression('E'['k']),side=2,line=2.5)
mtext('Temperature',side=1,line=2.5)

# plot(nwatl$NITRATE,nwatl$PMB,cex=0.3,xlim=c(0,20),ylim=c(0,15))
# plot(nwatl$NITRATE,nwatl$ALPHA,xlim=c(0,20),cex=0.3,ylim=c(0,0.2))
# plot(nwatl$NITRATE,nwatl$EK,cex=0.3,xlim=c(0,20))
# mtext('Nitrate',side=1,line=2.5)

plot(log10(nwatl$NITRATE+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.1,20)),ylim=c(0,15))
plot(log10(nwatl$NITRATE+0.1),nwatl$ALPHA,xlim=log10(c(0.1,20)),cex=0.3,ylim=c(0,0.2))
plot(log10(nwatl$NITRATE+0.1),nwatl$EK,cex=0.3,xlim=log10(c(0.1,20)))
mtext('log10 Nitrate',side=1,line=2.5)

# plot(nwatl$TCHL,nwatl$PMB,cex=0.3,xlim=c(0,25),ylim=c(0,15))
# plot(nwatl$TCHL,nwatl$ALPHA,xlim=c(0,25),cex=0.3,ylim=c(0,0.2))
# plot(nwatl$TCHL,nwatl$EK,cex=0.3,xlim=c(0,25))
# mtext('Chlorophyll',side=1,line=2.5)

plot(log10(nwatl$TCHL+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.1,25)),ylim=c(0,15))
plot(log10(nwatl$TCHL+0.1),nwatl$ALPHA,xlim=log10(c(0.1,25)),cex=0.3,ylim=c(0,0.2))
plot(log10(nwatl$TCHL+0.1),nwatl$EK,cex=0.3,xlim=log10(c(0.1,25)))
mtext('log10 Chlorophyll',side=1,line=2.5)

# plot(nwatl$Imld_clim,nwatl$PMB,cex=0.3,xlim=c(0,20),ylim=c(0,15))
# plot(nwatl$Imld_clim,nwatl$ALPHA,xlim=c(0,20),cex=0.3,ylim=c(0,0.2))
# plot(nwatl$Imld_clim,nwatl$EK,cex=0.3)
# mtext('Chlorophyll',side=1,line=2.5)

plot(nwatl$DEPTH,nwatl$PMB,cex=0.3,ylim=c(0,15))
plot(nwatl$DEPTH,nwatl$ALPHA,cex=0.3,ylim=c(0,0.2))
plot(nwatl$DEPTH,nwatl$EK,cex=0.3)
mtext('Depth',side=1,line=2.5)

plot(nwatl$DEPTH*nwatl$kd_clim,nwatl$PMB,cex=0.3,ylim=c(0,15),xlim=c(0,6))
plot(nwatl$DEPTH*nwatl$kd_clim,nwatl$ALPHA,cex=0.3,ylim=c(0,0.2),xlim=c(0,6))
plot(nwatl$DEPTH*nwatl$kd_clim,nwatl$EK,cex=0.3,xlim=c(0,6))
mtext('Optical Depth',side=1,line=2.5)

# plot(nwatl$PAR_clim,nwatl$PMB,cex=0.3,ylim=c(0,15))
# plot(nwatl$PAR_clim,nwatl$ALPHA,cex=0.3,ylim=c(0,0.2))
# plot(nwatl$PAR_clim,nwatl$EK,cex=0.3)
# mtext('Chlorophyll',side=1,line=2.5)
# 
# plot(nwatl$PAR_clim*exp(-nwatl$kd_clim*nwatl$DEPTH),nwatl$PMB,cex=0.3,ylim=c(0,15))
# plot(nwatl$PAR_clim*exp(-nwatl$kd_clim*nwatl$DEPTH),nwatl$ALPHA,cex=0.3,ylim=c(0,0.2))
# plot(nwatl$PAR_clim*exp(-nwatl$kd_clim*nwatl$DEPTH),nwatl$EK,cex=0.3)
# mtext('Irradiance at Depth',side=1,line=2.5)


dev.off()


pdf('~/google/working/pi_parameters/plots/env_scatter_atl.pdf',height=6,width=7/2)
par(mfcol=c(3,1),mar=c(2,2,2,2),oma=c(2,3,2,2))
plot(nwatl$NITRATE,nwatl$TEMP,ylim=c(-2,30),xlim=c(0,20),pch=19,cex=0.5)
mtext(side=1,'Nitrate',line=2.5)
mtext(side=2,'Temperature',line=2.5)
plot(nwatl$NITRATE,nwatl$TCHL,ylim=c(0,30),xlim=c(0,20),pch=19,cex=0.5)
mtext(side=1,'Nitrate',line=2.5)
mtext(side=2,'Chlorophyll',line=2.5)
plot(nwatl$TEMP,nwatl$TCHL,ylim=c(0,30),xlim=c(-2,30),pch=19,cex=0.5)
mtext(side=1,'Temperature',line=2.5)
mtext(side=2,'Chlorophyll',line=2.5)
dev.off()


###########################################################################
##--TEMPERATURE DEPENDENCE OF PBM--########################################
###########################################################################
dat <- nwatl[complete.cases(nwatl$TEMP),]

dat$NGROUP   <- cut_number(dat$NITRATE,4,labels=c('1','2','3','4')) #create nitrate groups
dat$CHLGROUP <- cut_number(dat$TCHL,4,labels=c('1','2','3','4')) #create nitrate groups
dat$ODGROUP  <- cut_number(dat$OPTDEPTH,4,labels=c('1','2','3','4')) #create nitrate groups
dat$PARGROUP <- cut_number(dat$PAR_depth_clim,4,labels=c('1','2','3','4')) #create nitrate groups

NGROUPS   <- c(0.1,0.16,0.636,2.54,27.7)
CHLGROUPS <- c(0.0332,0.56,1.21,2.81,105)
ODGROUPS  <- c(0.1,0.683,1.22,2.58,8.69)
PARGROUPS <- c(0.00536,2.52,8.18,17.6,51.4)

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

pdf('~/google/working/pi_parameters/plots/PMB_temperature_groups.pdf',height=6,width=12)
par(mfrow=c(2,3),mar=c(2,2,2,4),oma=c(2,3,2,2))
  plot(dat$TEMP, dat$PMB,cex=0.5)
  fit  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat, start=list(a=2.5,b=0.06,z=13,w=14))
  #fit  <- nls(PMB ~ tT(TEMP,a,bz,w), data=dat, start=list(z=13,w=14))
  lines(seq(0,30,0.01),predict(fit,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
  lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  
  legend(legend=c('Global Eppley-like', 'Behrenfeld & Falkowski'),'topleft',lty=1,col=c('red','blue'),bty='n')
  
  plot(dat$TEMP, dat$PMB, col=dat$NGROUP,pch=1,cex=0.5)
  image.plot(legend.only=TRUE,matrix(log10(dat$NITRATE)),col=1:4,breaks=log10(NGROUPS))
  mtext('Grouped by nitrate')
  FITS_NGROUP <- fits(dat,'NGROUP')

  plot(dat$TEMP, dat$PMB, col=dat$CHLGROUP,cex=0.5)
  image.plot(legend.only=TRUE,matrix(log10(dat$TCHL)),col=1:4,breaks=log10(CHLGROUPS))
  mtext('Grouped by chlorophyll')
  FITS_CHLGROUP <- fits(dat,'CHLGROUP')

  plot(dat$TEMP, dat$PMB, col=dat$ODGROUP,cex=0.5)
  image.plot(legend.only=TRUE,matrix(log10(dat$OPTDEPTH)),col=1:4,breaks=log10(ODGROUPS))
  mtext('Grouped by optical depth')
  FITS_ODGROUP <- fits(dat,'ODGROUP')

  plot(dat$TEMP, dat$PMB, col=dat$PARGROUP,cex=0.5)
  image.plot(legend.only=TRUE,matrix(log10(dat$PAR_depth_clim)),col=1:4,breaks=log10(PARGROUPS))
  mtext('Grouped by PAR')
  FITS_PARGROUP <- fits(dat,'PARGROUP')
  
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
fit <- lm(PMB ~ TEMP + NITRATE + TCHL + OPTDEPTH, data=nwatl)
fit <- lm(PMB ~ TEMP + log10(NITRATE+0.1) + log10(TCHL) + OPTDEPTH, data=nwatl)

pdf('~/google/working/pi_parameters/plots/lm_fits_atl.pdf',height=6,width=6)
  avPlots(fit,id=FALSE)
dev.off()

pdf('~/google/working/pi_parameters/plots/lm_fits_atl_cor.pdf',height=4,width=4)
  plot(nwatl$PMB, predict(fit, newdata=nwatl),xlim=c(0,10),ylim=c(0,10),cex=0.5,xlab='',ylab='')
  mtext(side=1,line=2.5, 'Observed'); mtext(side=2,line=2.5, 'Predicted') 
  mtext(bquote('cor = '*.(round(cor(nwatl$PMB,predict(fit,newdata=nwatl),use='pairwise.complete.obs'),3))),line=-1.5)
  abline(0,1) 
dev.off()

#####################################################################
## GAM FITS #########################################################
#####################################################################
pdf('~/google/working/pi_parameters/plots/gam_fits_atl.pdf',height=5,width=11)
sp <- 1
par(mfrow=c(2,5),mar=c(2,2,2,2),oma=c(2,2,2,2))
datt <- data.frame(PMB=dat$PMB, NITRATE=dat$NITRATE, TEMP=dat$TEMP, TCHL=dat$TCHL, OPTDEPTH=dat$OPTDEPTH)
datt <- datt[complete.cases(datt),]
samp <- sample(1:nrow(datt),size=nrow(datt)/2)
train <- datt[samp,]
test  <- datt[-samp,]

fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPTDEPTH,sp=sp), data=train)
plot.gam(fit,select=1, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Temperature',side=1,line=2.5) 
plot.gam(fit,select=2, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Nitrate',side=1,line=2.5)
plot.gam(fit,select=3, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Chlorophyll',side=1,line=2.5)
plot.gam(fit,select=4, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Optical depth',side=1,line=2.5)
plot(test$PMB,predict(fit,newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
mtext(side=1,'Observed',line=2.5); mtext(side=2,'Predicted',line=2.5)
mtext(bquote('cor = '*.(round(cor(dat$PMB,predict(fit,newdata=dat),use='pairwise.complete.obs'),3))),line=-1.5)
abline(0,1)

datt <- data.frame(PMB=dat$PMB, NITRATE=log10(dat$NITRATE+0.1), TEMP=dat$TEMP, TCHL=log10(dat$TCHL), OPDEPTH=log10(dat$OPTDEPTH+0.1))
datt <- datt[complete.cases(datt),]
samp <- sample(1:nrow(datt),size=nrow(datt)/2)
train <- datt[samp,]
test  <- datt[-samp,]

fit <- gam(PMB ~ s(TEMP,sp=sp) + s(NITRATE,sp=sp) + s(TCHL,sp=sp) + s(OPDEPTH,sp=sp), data=train)
plot.gam(fit,select=1, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('Temperature',side=1,line=2.5) 
plot.gam(fit,select=2, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('log Nitrate',side=1,line=2.5)
plot.gam(fit,select=3, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('log Chlorophyll',side=1,line=2.5)
plot.gam(fit,select=4, residuals=TRUE); mtext('Effect Size',side=2,line=2.5); mtext('log Optical depth',side=1,line=2.5)
plot(test$PMB,predict(fit,newdata=test),xlim=c(0,10),ylim=c(0,10),cex=0.5)
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

pdf('~/google/working/pi_parameters/plots/rf_fits_atl.pdf',height=8,width=6)
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



order = findInterval(nwatl$DEPTH, sort(nwatl$DEPTH))
nwatl$col <- viridis(nrow(nwatl))[order]

pdf('~/google/working/pi_parameters/plots/NWAtl_parameters_depth_lat.pdf',height=7,width=7)
par(mfrow=c(3,1),mar=c(2,2,2,8),oma=c(3,3,3,3))
#plot(nwatl$LAT,nwatl$PMB,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
plot(nwatl$LAT,nwatl$PMB,col=nwatl$col,pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('P'['m']^'B'),line=2.5)
image.plot(legend.only=TRUE, matrix(nwatl$DEPTH),col=viridis(20))
plot(nwatl$LAT,nwatl$ALPHA,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote(alpha^'B'),line=2.5)
plot(nwatl$LAT,nwatl$EK,col=viridis(nrow(nwatl))[order],pch=19,xlim=c(35,70),cex=0.5)
mtext(side=2,bquote('E'['k']),line=2.5)
mtext('Latitude',side=1,line=2.5)
dev.off()
