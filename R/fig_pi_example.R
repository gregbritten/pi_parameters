
PBmax <- 1
alpha <- 0.1
Iin <- seq(0,60)
Ik <- PBmax/alpha

pdf('plots/PI_example.pdf',height=4.5,width=6)
par(cex.axis=1)
plot(Iin, PBmax*(1-exp(-Iin/Ik)),ylim=c(0,1.3),type='l',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
  axis(side=1,at=c(0,Ik,60),labels=c('0',expression(italic('I'['k'])),expression(italic('I'['sat']))),lwd=1.5)
  axis(side=2,at=c(0,PBmax,1.3),labels=c('0',expression(italic('P'['max']^'B')),NA),lwd=1.5)
  abline(0,alpha,lty=2,lwd=1.5)
  abline(h=PBmax,lty=2,lwd=1.5)
  abline(v=Ik,lty=3,lwd=1.5)
  mtext(side=1,expression('Irradiance ['*italic(mu)*'E/m'^3*'/s]'),line=2.25)
  mtext(side=2,expression('Rate of Photosynthesis [mmolC/m'^3*'/s]'),line=2.5)
dev.off()
