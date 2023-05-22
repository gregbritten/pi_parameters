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

