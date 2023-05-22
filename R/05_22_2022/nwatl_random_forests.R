library(mgcv)
library(randomForest)

##--data processing--###########
dd <- nwa_d %>% filter(complete.cases(sst) & complete.cases(chl) & complete.cases(kd_490) & complete.cases(E_surf))
samp <- sample(1:nrow(dd),size=nrow(dd)/2)
dd_train <- dd[samp,]
dd_test  <- dd[-samp,]

dm <- nwa_m %>% filter(complete.cases(sst) & complete.cases(chl) & complete.cases(kd_490) & complete.cases(E_surf))
samp <- sample(1:nrow(dm),size=nrow(dm)/2)
dm_train <- dm[samp,]
dm_test  <- dm[-samp,]


##--PBMAX--################################  
#fit_rf_d_pbm  <- randomForest(PBmax ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dd_train)
#fit_rf_m_pbm  <- randomForest(PBmax ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dm_train)
fit_rf_d_pbm  <- randomForest(PBmax ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dd_train)
fit_rf_m_pbm  <- randomForest(PBmax ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dm_train)

pdf('~/dropbox/working/pi_parameters/github/plots/PBmax_rf.pdf',height=5,width=5)
par(mfrow=c(2,2),mar=c(2,2,2,2),oma=c(4,2,2,2))
lims <- c(0,10)
plot(dd_test$PBmax,as.numeric(predict(fit_rf_d_pbm,newdata=dd_test)),xlim=lims,ylim=lims)
abline(0,1); mtext(paste0('cor = ',paste(round(cor(dd_test$PBmax,as.numeric(predict(fit_rf_d_pbm,newdata=dd_test))),3))))
mtext(side=1,line=2.5,'Observed'); mtext(side=2,line=2.5,'Predicted') 
plot(dm_test$PBmax,as.numeric(predict(fit_rf_m_pbm,newdata=dm_test)),xlim=lims,ylim=lims)
abline(0,1); mtext(paste0('cor = ',paste(round(cor(dm_test$PBmax,as.numeric(predict(fit_rf_m,newdata=dm_test))),3))))
mtext(side=1,line=2.5,'Observed')

barplot(t(fit_rf_d_pbm$importance)/sum(fit_rf_d_pbm$importance),ylim=c(0,0.4),las=2)
mtext(side=2,line=2.5,'Variable importance')
barplot(t(fit_rf_m_pbm$importance)/sum(fit_rf_m_pbm$importance),ylim=c(0,0.4),las=2)
mtext(outer=TRUE,expression('P'['M']^'B'))
dev.off()

##--ALPHA--##################################
#fit_rf_d_alpha  <- randomForest(alpha ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dd_train)
#fit_rf_m_alpha  <- randomForest(alpha ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dm_train)
fit_rf_d_alpha  <- randomForest(alpha ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dd_train)
fit_rf_m_alpha  <- randomForest(alpha ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dm_train)

#lims <- c(50,200)
lims <- c(0,0.1)
pdf('~/dropbox/working/pi_parameters/github/plots/alpha_rf.pdf',height=5,width=5)
par(mfrow=c(2,2),mar=c(2,2,2,2),oma=c(4,2,2,2))
#plot(dd_test$PBmax,as.numeric(predict(fit_gam_d,newdata=dd_test)),ylim=lims,xlim=lims)
#  abline(0,1); mtext(paste(round(cor(dd_test$PBmax,as.numeric(predict(fit_gam_d,newdata=dd_test))),3)))
plot(dd_test$alpha,as.numeric(predict(fit_rf_d_alpha,newdata=dd_test)),xlim=lims,ylim=lims)
abline(0,1); mtext(paste0('cor = ',paste(round(cor(dd_test$alpha,as.numeric(predict(fit_rf_d_alpha,newdata=dd_test))),3))))
mtext(side=1,line=2.5,'Observed'); mtext(side=2,line=2.5,'Predicted') 
plot(dm_test$alpha,as.numeric(predict(fit_rf_m_alpha,newdata=dm_test)),xlim=lims,ylim=lims)
abline(0,1); mtext(paste0('cor = ',paste(round(cor(dm_test$alpha,as.numeric(predict(fit_rf_m_alpha,newdata=dm_test))),3))))
mtext(side=1,line=2.5,'Observed')

barplot(t(fit_rf_d_alpha$importance)/sum(fit_rf_d_alpha$importance),ylim=c(0,0.4),las=2)
mtext(side=2,line=2.5,'Variable importance')
barplot(t(fit_rf_m_alpha$importance)/sum(fit_rf_m_alpha$importance),ylim=c(0,0.4),las=2)
mtext(outer=TRUE,expression(alpha['M']^'B'))
dev.off()

##--Ek--##################################
#fit_rf_d_ek  <- randomForest(Ek ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dd_train)
#fit_rf_m_ek  <- randomForest(Ek ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + longhurst + region + sat_par + Zeu, data=dm_train)
#fit_rf_d_ek  <- randomForest(Ek ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dd_train)
#fit_rf_m_ek  <- randomForest(Ek ~ sst + chl + kd_490 + E_surf + month + Ez + daylength + sat_par + Zeu, data=dm_train)

#lims <- c(50,200)
#lims <- c(25,200)
#par(mfrow=c(2,2),mar=c(2,2,1,1))
#plot(dd_test$PBmax,as.numeric(predict(fit_gam_d,newdata=dd_test)),ylim=lims,xlim=lims)
#  abline(0,1); mtext(paste(round(cor(dd_test$PBmax,as.numeric(predict(fit_gam_d,newdata=dd_test))),3)))
#plot(dd_test$Ek,as.numeric(predict(fit_rf_d_ek,newdata=dd_test)),xlim=lims,ylim=lims)
#abline(0,1); mtext(paste(round(cor(dd_test$Ek,as.numeric(predict(fit_rf_d_ek,newdata=dd_test))),3)))

#plot(dm_test$Ek,as.numeric(predict(fit_rf_m_ek,newdata=dm_test)),xlim=lims,ylim=lims)
#abline(0,1); mtext(paste(round(cor(dm_test$Ek,as.numeric(predict(fit_rf_m_ek,newdata=dm_test))),3)))

#barplot(t(fit_rf_d_ek$importance)/sum(fit_rf_d_ek$importance),ylim=c(0,0.4),las=2)
#barplot(t(fit_rf_m_ek$importance)/sum(fit_rf_m_ek$importance),ylim=c(0,0.4))

  
  
  
  
  
  
  
  
  
  
  
  