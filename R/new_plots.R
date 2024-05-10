library(mgcv)
library(randomForestExplainer)
library(viridis)

i <- 1

FITS <- list()
RSQ  <- matrix(NA,nrow=3,ncol=365)


for(i in 1:365){
  print(i)
  d <- D_nc[[i]] %>% filter(complete.cases(sst,par,depth,nano_pico,region),depth<=40)

  fit_g_Ek    <- randomForest(Ek    ~ sst + par + depth + nano_pico + region, data=d, importance=TRUE) 
  fit_g_PBmax <- randomForest(PBmax ~ sst + par + depth + nano_pico + region, data=d, importance=TRUE) 
  fit_g_alpha <- randomForest(alpha ~ sst + par + depth + nano_pico + region, data=d, importance=TRUE) 

  FITS[[i]] <- list(fit_g_Ek,fit_g_PBmax,fit_g_alpha)
  RSQ[1,i] <- cor(fit_g_Ek$predicted,d$Ek)^2
  RSQ[2,i] <- cor(fit_g_PBmax$predicted,d$PBmax)^2
  RSQ[3,i] <- cor(fit_g_alpha$predicted,d$alpha)^2
}



#############################################
#############################################
pdf('plots/RSQ.pdf',height=4,width=6.5)
par(mfrow=c(1,2),mar=c(2,1,2,1.5),oma=c(3,3,3,3))
plot(RSQ[1,],type='l')
abline(v=which(RSQ[1,]==max(RSQ[1,])),lty=2)
mtext(side=2,expression(italic('R'^2)),line=2.5,cex=1.2)
mtext(expression(italic('P'['max']^'B')),adj=0)

plot(RSQ[2,],type='l')
abline(v=which(RSQ[2,]==max(RSQ[2,])),lty=2)
mtext(side=1,outer=TRUE,'Days',line=0.5)
mtext(expression(italic('E'['k'])),adj=0)
dev.off()


par(mfrow=c(1,2))
plot_predict_interaction(FITS[[25]][[1]], d, 'sst','par')
plot_predict_interaction(FITS[[25]][[1]], d, 'sst','nano_pico')
plot_predict_interaction(FITS[[25]][[1]], d, 'par','nano_pico')

plot_predict_interaction(FITS[[25]][[2]], d, 'sst','par')
plot_predict_interaction(FITS[[25]][[2]], d, 'sst','nano_pico')
plot_predict_interaction(FITS[[25]][[2]], d, 'par','nano_pico')



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
plot(-999,xlim=c(0,80),ylim=c(0,200),xlab='',ylab='',xaxt='n'); axis(side=1,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='par',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=2,expression(italic('E'['k'])),line=2.5)
legend('topleft',legend=region_long,lty=1,col=cols,bty='n',cex=0.6)

##SST##
plot(-999,xlim=c(0,30),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1,labels=FALSE); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='sst',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}

##nano_pico##
plot(-999,xlim=c(0,10),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1,labels=FALSE); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='nano_pico',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}

##depth##
plot(-999,xlim=c(0,40),ylim=c(0,200),xlab='',ylab='',xaxt='n',yaxt='n'); axis(side=1,labels=FALSE); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[1]],x.var='depth',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}


##PAR##
plot(-999,xlim=c(0,80),ylim=c(2,5),xlab='',ylab=''); axis(side=1,labels=FALSE); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='par',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=2,expression(italic('P'['max']^'B')),line=2.5)
mtext(side=1,expression(italic('PAR')),line=2.5)

##SST##
plot(-999,xlim=c(0,30),ylim=c(2,5),xlab='',ylab='',yaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='sst',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('SST')),line=2.5)

##nano_pico##
plot(-999,xlim=c(0,10),ylim=c(2,5),xlab='',ylab='',yaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='nano_pico',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('f'['pico'])),line=2.5)

##depth##
plot(-999,xlim=c(0,40),ylim=c(2,5),xlab='',ylab='',yaxt='n'); axis(side=2,labels=FALSE)
for(i in 1:length(regions)){
  xx <- partialPlot(FITS[[25]][[2]],x.var='depth',pred.data=d %>% filter(region==regions[i]),plot=FALSE)
  lines(xx$x,xx$y,col=cols[i])
}
mtext(side=1,expression(italic('depth')),line=2.5)


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


