library(randomForest)
library(randomForestExplainer)
library(viridis)
library(lme4)
source('r/main_process_nc.R')

ndays         <- 100
FITS=FITS_lm  <- list()
RSQ=RSQ_lm    <- matrix(NA,nrow=3,ncol=ndays)

for(i in 1:ndays){
  print(i)
  d <- D_nc[[i]] %>% filter(complete.cases(sst,par,depth,pico,region),depth<=40)

  fit_g_Ek    <- randomForest(Ek    ~ sst + par + depth + pico + region, data=d, importance=TRUE) 
  fit_g_PBmax <- randomForest(PBmax ~ sst + par + depth + pico + region, data=d, importance=TRUE) 
  fit_g_alpha <- randomForest(alpha ~ sst + par + depth + pico + region, data=d, importance=TRUE) 

  FITS[[i]] <- list(fit_g_Ek,fit_g_PBmax,fit_g_alpha)
  RSQ[1,i] <- cor(fit_g_Ek$predicted,d$Ek)^2
  RSQ[2,i] <- cor(fit_g_PBmax$predicted,d$PBmax)^2
  RSQ[3,i] <- cor(fit_g_alpha$predicted,d$alpha)^2
  
  
  fit_g_Ek_lm    <- lmer(scale(Ek)    ~ scale(sst) + scale(par) + scale(depth) + scale(pico) + (1|region), data=d) 
  fit_g_PBmax_lm <- lmer(scale(PBmax) ~ scale(sst) + scale(par) + scale(depth) + scale(pico) + (1|region), data=d) 
  fit_g_alpha_lm <- lmer(scale(alpha) ~ scale(sst) + scale(par) + scale(depth) + scale(pico) + (1|region), data=d) 
  
  FITS_lm[[i]] <- list(fit_g_Ek_lm,fit_g_PBmax_lm,fit_g_alpha_lm)
  RSQ_lm[1,i] <- cor(predict(fit_g_Ek_lm),d$Ek)^2
  RSQ_lm[2,i] <- cor(predict(fit_g_PBmax_lm),d$PBmax)^2
  RSQ_lm[3,i] <- cor(predict(fit_g_alpha_lm),d$alpha)^2
}



#############################################
#############################################
pdf('plots/RSQ.pdf',height=4,width=6.5)
par(mfrow=c(1,2),mar=c(2,1,2,1.5),oma=c(3,3,3,3),cex.axis=0.8)
plot(RSQ[1,],type='l',ylim=c(0.3,0.8),bty='n')
lines(RSQ_lm[1,],lty=2)
abline(v=which(RSQ[1,]==max(RSQ[1,])),lty=1)
abline(v=which(RSQ_lm[1,]==max(RSQ_lm[1,])),lty=2)
mtext(side=2,expression(italic('R'^2)),line=2.5,cex=1.2)
mtext(expression(italic('a) P'['max']^'B')~'[mg C (mg chla)'^{-1}~'h'^{-1}*']'),adj=0)

plot(RSQ[2,],type='l',ylim=c(0.3,0.8),yaxt='n',cex.axis=0.8,bty='n'); axis(side=2,labels=NA)
abline(v=which(RSQ[2,]==max(RSQ[2,])),lty=1)
abline(v=which(RSQ_lm[2,]==max(RSQ_lm[2,])),lty=2)
lines(RSQ_lm[2,],lty=2)

mtext(side=1,outer=TRUE,'Days',line=0.5)
mtext(expression(italic('b) E'['k'])~'['*mu*'mol quanta m'^{-2}~'s'^{-1}*']'),adj=0)
dev.off()


#par(mfrow=c(1,2))
#plot_predict_interaction(FITS[[30]][[1]], d, 'sst','par')
#plot_predict_interaction(FITS[[30]][[1]], d, 'sst','nano_pico')
#plot_predict_interaction(FITS[[30]][[1]], d, 'par','nano_pico')

#plot_predict_interaction(FITS[[25]][[2]], d, 'sst','par')
#plot_predict_interaction(FITS[[25]][[2]], d, 'sst','nano_pico')
#plot_predict_interaction(FITS[[25]][[2]], d, 'par','nano_pico')



cols <- turbo(5)

###########################################
## PBmax ##################################
###########################################
pdf('plots/partial_effects.pdf',height=4,width=8)
par(mfrow=c(2,4),mar=c(0.5,0.5,0.5,0.5),oma=c(4,5,3,3))

###########################################
## PBmax ##################################
###########################################
##PAR##
plot(-999,xlim=c(0,80),ylim=c(2,5),xlab='',ylab='',xaxt='n'); axis(side=1,labels=FALSE); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='par',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=2,expression(italic('P'['max']^'B')~'[mg C (mg chla)'^{-1}~'h'^{-1}*']'),line=2.5,cex=0.75)
mtext('a)',adj=0.05,line=-1.5)
legend('topright',legend=region_long,lty=1,col=cols,bty='n',cex=0.8)

##SST##
plot(-999,xlim=c(-2,30),ylim=c(2,5),xlab='',ylab='',yaxt='n',xaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='sst',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext('b)',adj=0.05,line=-1.5)

##nano_pico##
plot(-999,xlim=c(0,0.8),ylim=c(2,5),xlab='',ylab='',yaxt='n',xaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='pico',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext('c)',adj=0.05,line=-1.5)

##depth##
plot(-999,xlim=c(0,40),ylim=c(2,5),xlab='',ylab='',yaxt='n',xaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='depth',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext('d)',adj=0.05,line=-1.5)


#########################
## Ek ###################
#########################
##PAR##
plot(-999,xlim=c(0,80),ylim=c(0,200),xlab='',ylab='',xaxt='n'); axis(side=1)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='par',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=2,expression(italic('E'['k'])~'['*mu*'mol quanta m'^{-2}~'s'^{-1}*']'),line=2.5,cex=0.75)
mtext(side=1,expression(italic('PAR')),line=2.5)
mtext('e)',adj=0.05,line=-1.5)

##SST##
plot(-999,xlim=c(-2,30),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='sst',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('SST')),line=2.5)
mtext('f)',adj=0.05,line=-1.5)

##nano_pico##
plot(-999,xlim=c(0,0.8),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='pico',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('c'['pico']*'/c'['total'])),line=2.5)
mtext('g)',adj=0.05,line=-1.5)

##depth##
plot(-999,xlim=c(0,40),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='depth',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('depth')),line=2.5)
mtext('h)',adj=0.05,line=-1.5)

dev.off()



###############################################
###############################################
plotlm <- function(fit,add=FALSE,ylims,labs){
  coef <- summary(fit)$coefficients[,1]
  ses  <- summary(fit)$coefficients[,2]
  n    <- length(coef)
  if(add==FALSE){
    #plot(1:n,coef,ylim=c(min(coef-2*ses),max(coef+2*ses)),pch=19,xaxt='n')
    plot(1:n,coef,pch=19,xaxt='n',ylim=ylims,xlim=c(1,n+0.5),xaxt='n',bty='n',yaxt='n',cex=0.8)
    segments(x0=1:n,x1=1:n,y0=coef-2*ses,y1=coef+2*ses)
  }
  if(add==TRUE){
    points(1:n+0.25,coef,ylim=c(min(coef-2*ses),max(coef+2*ses)),pch=19,col='red')
    segments(x0=1:n+0.25,x1=1:n+0.25,y0=coef-2*ses,y1=coef+2*ses,col='red')
  }
  abline(h=0,lty=2)
  #axis(side=1,at=1:n,labels=row.names(summary(fit)$coefficients),las=2)
  axis(side=1,at=1:n,labels=labs,las=2)
}





pdf('plots/lm_effects.pdf',height=3.75,width=7)
par(mfrow=c(1,2),mar=c(2,2,2,0),oma=c(4,4,2,2))
plotlm(FITS_lm[[30]][[1]],ylim=c(-0.8,0.8),labs=c(expression(italic("Intercept")),
                                              expression(italic("SST")),
                                              expression(italic("PAR")),
                                              expression(italic("Depth")),
                                              expression(italic("f"["pico"]))))
axis(side=2,at=seq(-0.8,0.8,0.2))
mtext(expression(italic('a) P'['max']^'B')~'[mg C (mg chla)'^{-1}~'h'^{-1}*']'),adj=0)

mtext(side=2,expression('Standardized Slope ['*Delta*'sdY/'*Delta*'sdX]'),line=2.5)
plotlm(FITS_lm[[30]][[2]],ylim=c(-0.8,0.8),labs=c(expression(italic("Intercept")),
                                                   expression(italic("SST")),
                                                   expression(italic("PAR")),
                                                   expression(italic("Depth")),
                                                   expression(italic("f"["pico"]))))
mtext(expression(italic('b) E'['k'])~'['*mu*'mol quanta m'^{-2}~'s'^{-1}*']'),adj=0)
dev.off()


##################################################
## PBmax vs. Ek ##################################
##################################################

rsqs <- numeric(5)
par(mfrow=c(1,1))
plot(-999,xlim=c(-0,15),ylim=c(0,500))
for(i in 1:length(regions)){
  dd <- d %>% filter(region==regions[i])
  points(dd$PBmax,dd$Ek,col=adjustcolor(cols[i],alpha.f=0.4),pch=19,lwd=0)
  fit <- lm(Ek ~ PBmax,data=dd)
  if(regions[i]%in%c('spac','scot')){
    PBmax_in <- seq(min(dd$PBmax),max(dd$PBmax)-2,length.out=1000)
  }else{PBmax_in <- seq(min(dd$PBmax),max(dd$PBmax+3),length.out=1000)}
  preds <- predict(fit,newdata=list(PBmax=PBmax_in))
  lines(PBmax_in,preds,col=cols[i],lwd=3)
  rsqs[i] <- round(summary(fit)$r.squared,3)
}
mtext(side=1,expression(italic('P'['max']^'B')),line=2.5,cex=1.5)
mtext(side=2,expression(italic(alpha^'B')),line=2.5,cex=1.5)

legend('topright',legend=c(bquote(.(region_long[1])~'r'^2~'='~.(rsqs[1])),
                           bquote(.(region_long[2])~'r'^2~'='~.(rsqs[2])),
                           bquote(.(region_long[3])~'r'^2~'='~.(rsqs[3])),
                           bquote(.(region_long[4])~'r'^2~'='~.(rsqs[4])),
                           bquote(.(region_long[5])~'r'^2~'='~.(rsqs[5]))),cex=0.7,bty='n',col=adjustcolor(cols,alpha.f=0.5),pch=19)


