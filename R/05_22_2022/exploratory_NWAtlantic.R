library(randomForest)
library(car)

rm(list=ls())

source('R/functions.r')

##--PROCESS DATA--################
#source('~/dropbox/working/pi_parameters/github/R/data_process.R')
nwatl <- read.csv('processed_data/nwatl.csv')

##--MAP WITH OBSERVATIONS--#############################
pdf('~/dropbox/working/pi_parameters/github/plots/north_atlantic_points.pdf',height=5.5,width=4.5)
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
maps::map(fill=TRUE,col='grey',xlim=c(-100,-20),ylim=c(30,80),resolution=0)
  points(nwatl$LON,nwatl$LAT,cex=0.2,col='red',pch=4,lwd=0.5)
  box()
  axis(side=1); axis(side=2)
  mtext(side=1,line=2.5,"Longitude")
  mtext(side=2,line=2.5,"Latitude")
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
pdf('~/dropbox/working/pi_parameters/github/plots/pmb_alpha_ek_scatter_atl.pdf',height=4,width=9.5)
par(mfcol=c(2,6),mar=c(0,0,0,0),oma=c(6,6,4,4),cex.axis=0.8)
fit1  <- nls(PMB ~ tT(TEMP,a,b,z,w), data=nwatl, start=list(a=2.5,b=0.06,z=13,w=14))
fit2  <- lm(PMB ~ TEMP + I(TEMP^2) + I(TEMP^3), data=nwatl) 

plot(nwatl$TEMP,nwatl$PMB,cex=0.3,xlim=c(-2,30),ylim=c(0,15),xaxt='n')
  mtext(expression('P'['m']^{'B'}),side=2,line=2.5)
  lines(seq(0,30,0.01),predict(fit1,newdata=list(TEMP=seq(0,30,0.01))),col='red')  
  lines(seq(0,30,0.01),predict(fit2,newdata=list(TEMP=seq(0,30,0.01))),col='green')  
  lines(seq(0,30,0.01),tT_behren(seq(0,30,0.01)),col='blue')  
  legend(legend=c('BF97','Epply-like', '3rd ord. poly'),'topleft',lty=1,col=c('blue','red','green'),bty='n')
plot(nwatl$TEMP,nwatl$ALPHA,xlim=c(-2,30),cex=0.3,ylim=c(0,0.21))
  mtext(expression(alpha^{'B'}),side=2,line=2.5)
  mtext('Temperature',side=1,line=2.5)
plot(log10(nwatl$NITRATE+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.07,20)),ylim=c(0,15),xaxt='n',yaxt='n')
plot(log10(nwatl$NITRATE+0.1),nwatl$ALPHA,xlim=log10(c(0.07,20)),cex=0.3,ylim=c(0,0.21),yaxt='n')
  mtext(expression('log'['10']~'(Nitrate)'),side=1,line=2.5)
plot(log10(nwatl$TCHL+0.1),nwatl$PMB,cex=0.3,xlim=log10(c(0.1,25)),ylim=c(0,15),xaxt='n',yaxt='n')
plot(log10(nwatl$TCHL+0.1),nwatl$ALPHA,xlim=log10(c(0.1,25)),cex=0.3,ylim=c(0,0.21),yaxt='n')
  mtext(expression('log'['10']~'(Chlorophyll)'),side=1,line=2.5)
plot(nwatl$DEPTH,nwatl$PMB,cex=0.3,ylim=c(0,15),yaxt='n',xaxt='n',xlim=c(0,130))
plot(nwatl$DEPTH,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),yaxt='n',xlim=c(0,10))
  mtext('Depth',side=1,line=2.5)
plot(nwatl$PARc,nwatl$PMB,cex=0.3,ylim=c(0,15),xlim=c(0,60),xaxt='n',yaxt='n')
plot(nwatl$PARc,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),xlim=c(0,60),yaxt='n')
  mtext('PAR',side=1,line=2.5)
plot(nwatl$DEPTH*nwatl$KDc,nwatl$PMB,cex=0.3,ylim=c(0,15),xlim=c(0,6),xaxt='n',yaxt='n')
plot(nwatl$DEPTH*nwatl$KDc,nwatl$ALPHA,cex=0.3,ylim=c(0,0.21),xlim=c(0,6),yaxt='n')
  mtext('Optical Depth',side=1,line=2.5)
dev.off()

###############################################################################
## ENVIRONMENTAL SCATTER PLOTS ################################################
###############################################################################
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
