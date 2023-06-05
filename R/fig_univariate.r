library(RColorBrewer)
library(colorRamps)

source('r/process_csvs.r') ##--data processing--###########

Iin <- seq(0,80,0.1)

regions <- c('scot','lab','spac','tas','ice')
region_long <- c('Scotian Shelf','Labrador Sea','South Pacific','Southern Ocean','Iceland Shelf')

#col_typs <- brewer.pal(5,'Dark2')
col_typs <- turbo(5)
cols <- matrix(NA,nrow=30,ncol=length(regions))
#for(i in 1:5) cols[,i] <- colorRampPalette(c(paste(col_typs[i]),'white'))(30)
for(i in 1:5) cols[,i] <- colorRampPalette(c(paste(col_typs[i])))(30)

plot_univariate <- function(chl=FALSE){
  cors=bs <- matrix(NA,nrow=30,ncol=length(regions))
  for(j in 1:length(regions)){
    for(i in 1:30){
      if(chl==FALSE) d <- D[[i]] %>% filter(region==regions[j])
      if(chl==TRUE) d <- D[[i]] %>% filter(region==regions[j], complete.cases(chl))
      cors[i,j] <- cor(d$Ek,d$par,use='complete.obs')
      bs[i,j] <- summary(lm(Ek~par,data=d))$coefficients[2,1]
    }
  }
  
  par(mfrow=c(1,3))
  plot(-999,xlim=c(0,80),ylim=c(0,200),xlab='',ylab='')
  mtext(side=1,line=2.5,expression(italic(bar(I)[0])~'[units]'))
  mtext(side=2,line=2.5,expression(italic(I['k'])))
  mtext('a)',adj=0)
  for(j in 1:length(regions)){
    #d <- D[[1]] %>% filter(region==regions[j])
    #lines(Iin,predict(lm(Ek ~ par, data=d),newdata=list(par=Iin)),col=cols[1,j],lwd=2)
    for(i in 1:30){
      if(chl==FALSE) d <- D[[i]] %>% filter(region==regions[j])
      if(chl==TRUE) d <- D[[i]] %>% filter(region==regions[j],complete.cases(chl))
      lines(Iin,predict(lm(Ek ~ par, data=d),newdata=list(par=Iin)),col=adjustcolor(cols[i,j],alpha.f=0.2),lty=1)
    }
  }
  
  plot(-999,xlim=c(0,30),ylim=c(-2,4),xlab='',ylab='')
  mtext('b)',adj=0)
  for(i in 1:length(regions)) lines(bs[,i],col=col_typs[i],lwd=1.5)
  legend(18,3,col=col_typs,lty=1,legend=region_long,bty='n')
  mtext(side=1,'Averaging Timescale [days]',line=2.5)
  mtext(side=2,expression(italic(b)*' = '*Delta*italic(I['k'])*'/'*Delta*italic(bar(I)[0])),line=2.5)
  
  plot(-999,xlim=c(0,30),ylim=c(-0.3,0.3),xlab='',ylab='')
  mtext('c)',adj=0)
  for(i in 1:length(regions)) lines(cors[,i],col=col_typs[i],lwd=1.5)
  #legend(18,0.05,col=1:5,lty=1,legend=region_long,bty='n')
  mtext(side=1,'Averaging Timescale [days]',line=2.5)
  mtext(side=2,'Correlation Coefficient',line=2.5)
}


pdf('plots/univariate_chl_false.pdf',height=4,width=12)
plot_univariate(chl=FALSE)
dev.off()

pdf('plots/univariate_chl_true.pdf',height=4,width=12)
plot_univariate(chl=TRUE)
dev.off()



#####################################################################
pdf('plots/scatter_Ez_Esurf.pdf',height=10,width=3)
par(mfrow=c(5,2))
for(i in 1:5){
  d <- D[[1]] %>% filter(region==regions[i])
  plot(d$Ez,d$Ek)
  plot(d$E_surf,d$Ek)
}
dev.off()

#i = i+1

