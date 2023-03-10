###########################################################################
##--TEMPERATURE DEPENDENCE OF PBM--########################################
###########################################################################
dat <- nwatl[complete.cases(nwatl$TEMP),]

dat$NGROUP   <- cut_number(dat$NITRATE,3,labels=c('1','2','3'))#,'4')) #create nitrate groups
dat$CHLGROUP <- cut_number(dat$TCHL,3,labels=c('1','2','3'))#,'4')) #create nitrate groups
dat$ODGROUP  <- cut_number(dat$OPTDEPTH,3,labels=c('1','2','3'))#,'4')) #create nitrate groups
dat$PARGROUP <- cut_number(dat$PARc_z,3,labels=c('1','2','3'))#,'4')) #create nitrate groups

NGROUPS   <- c(0.1,0.16,0.636,2.54,27.7)
CHLGROUPS <- c(0.0332,0.56,1.21,2.81,105)
ODGROUPS  <- c(0.1,0.683,1.22,2.58,8.69)
PARGROUPS <- c(0.00536,2.52,8.18,17.6,51.4)

#dat$NGROUP   <- cut_number(dat$NITRATE,3,labels=c('1','2','3')) #create nitrate groups
#dat$CHLGROUP <- cut_number(dat$TCHL,3,labels=c('1','2','3')) #create nitrate groups

####################################################################
## TEMPERATURE AND CHLOROPHYLL GROUPS ##############################
####################################################################
fits <- fitgroup <- function(dat,group){
  if(group=='NGROUP'){dat1 <- dat[dat$NGROUP==1,]; dat2   <- dat[dat$NGROUP==2,]; dat3   <- dat[dat$NGROUP==3,]}#;   dat4 <- dat[dat$NGROUP==4,]}
  if(group=='CHLGROUP'){dat1 <- dat[dat$CHLGROUP==1,]; dat2 <- dat[dat$CHLGROUP==2,]; dat3 <- dat[dat$CHLGROUP==3,]}#; dat4 <- dat[dat$CHLGROUP==4,]}
  if(group=='ODGROUP'){dat1 <- dat[dat$ODGROUP==1,]; dat2 <- dat[dat$ODGROUP==2,]; dat3 <- dat[dat$ODGROUP==3,]}#; dat4 <- dat[dat$ODGROUP==4,]}
  if(group=='PARGROUP'){dat1 <- dat[dat$PARGROUP==1,]; dat2 <- dat[dat$PARGROUP==2,]; dat3 <- dat[dat$PARGROUP==3,]}#; dat4 <- dat[dat$PARGROUP==4,]}
  
  fit1 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat1, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  fit2 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat2, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  fit3 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat3, start=list(a=2.5,b=0.06,z=13,w=14),control=list(warnOnly=TRUE))
  #fit4 <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat4, start=list(a=2.5,b=0.06,z=8,w=14),control=list(warnOnly=TRUE))
  
  lines(seq(0,30,0.01),predict(fit1,newdata=list(TEMP=seq(0,30,0.01))),col=1)  
  lines(seq(0,30,0.01),predict(fit2,newdata=list(TEMP=seq(0,30,0.01))),col=2)  
  lines(seq(0,30,0.01),predict(fit3,newdata=list(TEMP=seq(0,30,0.01))),col=3)  
  #lines(seq(0,30,0.01),predict(fit4,newdata=list(TEMP=seq(0,30,0.01))),col=4) 
  
  fits <- list(fit1,fit2,fit3)#,fit4)
  return(fits)
}
#dat1 <- dat[dat$NGROUP==1,]; dat2 <- dat[dat$NGROUP==2,]; dat3 <- dat[dat$NGROUP==3,]; 

pdf('~/google/working/pi_parameters/plots/PMB_temperature_groups.pdf',height=6,width=12)
par(mfrow=c(2,3),mar=c(2,2,2,4),oma=c(2,3,2,2))
plot(dat$TEMP, dat$PMB,cex=0.7,pch=20,ylim=c(0,15))
fit  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat, start=list(a=2.5,b=0.06,z=13,w=14))
#fit  <- nls(PMB ~ tT(TEMP,a,bz,w), data=dat, start=list(z=13,w=14))
lines(seq(0,30,0.01),predict(fit,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  

plot(dat$TEMP, dat$ALPHA,cex=0.7,pch=20,ylim=c(0,0.2))
fit  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=dat, start=list(a=2.5,b=0.06,z=13,w=14))
#fit  <- nls(PMB ~ tT(TEMP,a,bz,w), data=dat, start=list(z=13,w=14))
lines(seq(0,30,0.01),predict(fit,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  


legend(legend=c('Global Eppley-like', 'Behrenfeld & Falkowski'),'topleft',lty=1,col=c('red','blue'),bty='n')

plot(dat$TEMP, dat$PMB, col=dat$NGROUP,cex=0.7,pch=20,ylim=c(0,15))


image.plot(legend.only=TRUE,matrix(log10(dat$NITRATE)),col=1:4,breaks=log10(NGROUPS))
mtext('Grouped by nitrate')
FITS_NGROUP <- fits(dat,'NGROUP')

plot(dat$TEMP, dat$PMB, col=dat$CHLGROUP,cex=0.7,pch=20,ylim=c(0,15))
#image.plot(legend.only=TRUE,matrix(log10(dat$TCHL)),col=1:4,breaks=log10(CHLGROUPS))
mtext('Grouped by chlorophyll')
FITS_CHLGROUP <- fits(dat,'CHLGROUP')

plot(dat$TEMP, dat$PMB, col=dat$ODGROUP,cex=0.7,pch=20,ylim=c(0,15))
#image.plot(legend.only=TRUE,matrix(log10(dat$OPTDEPTH)),col=1:4,breaks=log10(ODGROUPS))
mtext('Grouped by optical depth')
FITS_ODGROUP <- fits(dat,'ODGROUP')

plot(dat$TEMP, dat$PMB, col=dat$PARGROUP,cex=0.7,pch=20,ylim=c(0,15))
#image.plot(legend.only=TRUE,matrix(log10(dat$PAR_depth_clim)),col=1:4,breaks=log10(PARGROUPS))
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

