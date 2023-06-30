library(randomForests)
source('r/process_csvs.r') ##--data processing--###########
source('r/process_')
######################################################################
## LINEAR REGRESSION #################################################
######################################################################
#fits_Ek_chl_T_lm    <- fit_lm(parm='Ek',chl=TRUE,full=TRUE)
#fits_alpha     <- fit_rf(parm='alpha',chl=FALSE,full=TRUE)
#fits_alpha_chl_T_lm <- fit_lm(parm='alpha',chl=TRUE,full=TRUE)
#fits_PBmax     <- fit_rf(parm='PBmax',chl=FALSE,full=TRUE)
#fits_PBmax_chl_T_lm <- fit_lm(parm='PBmax',chl=TRUE,full=TRUE)

######################################################################
## RANDOM FORESTS ####################################################
######################################################################
fits_Ek_chl_T_365    <- fit_rf(parm='Ek',   chl=TRUE,full=TRUE,ntime=365)
fits_alpha_chl_T_365 <- fit_rf(parm='alpha',chl=TRUE,full=TRUE,ntime=365)
fits_PBmax_chl_T_365 <- fit_rf(parm='PBmax',chl=TRUE,full=TRUE,ntime=365)

fits_Ek_chl_F_365    <- fit_rf(parm='Ek',   chl=TRUE,full=FALSE,ntime=365)
fits_alpha_chl_F_365 <- fit_rf(parm='alpha',chl=TRUE,full=FALSE,ntime=365)
fits_PBmax_chl_F_365 <- fit_rf(parm='PBmax',chl=TRUE,full=FALSE,ntime=365)


saveRDS(fits_Ek_chl_T_365,   'results/fits_Ek_chl_T_365.rds')
saveRDS(fits_alpha_chl_T_365,'results/fits_alpha_chl_T_365.rds')
saveRDS(fits_PBmax_chl_T_365,'results/fits_PBmax_chl_T_365.rds')

saveRDS(fits_Ek_chl_F_365,   'results/fits_Ek_chl_F_365.rds')
saveRDS(fits_alpha_chl_F_365,'results/fits_alpha_chl_F_365.rds')
saveRDS(fits_PBmax_chl_F_365,'results/fits_PBmax_chl_F_365.rds')



