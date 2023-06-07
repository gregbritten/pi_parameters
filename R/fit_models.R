source('r/process_csvs.r') ##--data processing--###########

######################################################################
## LINEAR REGRESSION #################################################
######################################################################
fits_Ek_chl_T_lm    <- fit_lm(parm='Ek',chl=TRUE,full=TRUE)
#fits_alpha     <- fit_rf(parm='alpha',chl=FALSE,full=TRUE)
fits_alpha_chl_T_lm <- fit_lm(parm='alpha',chl=TRUE,full=TRUE)
#fits_PBmax     <- fit_rf(parm='PBmax',chl=FALSE,full=TRUE)
fits_PBmax_chl_T_lm <- fit_lm(parm='PBmax',chl=TRUE,full=TRUE)



######################################################################
## RANDOM FORESTS ####################################################
######################################################################
#fits_Ek        <- fit_rf(parm='Ek',chl=FALSE,full=FALSE)
#fits_Ek_chl_F    <- fit_rf(parm='Ek',chl=TRUE,full=FALSE)
#fits_alpha     <- fit_rf(parm='alpha',chl=FALSE,full=FALSE)
#fits_alpha_chl_F <- fit_rf(parm='alpha',chl=TRUE,full=FALSE)
#fits_PBmax     <- fit_rf(parm='PBmax',chl=FALSE,full=FALSE)
#fits_PBmax_chl_F <- fit_rf(parm='PBmax',chl=TRUE,full=FALSE)

#fits_Ek        <- fit_rf(parm='Ek',chl=FALSE,full=TRUE)
fits_Ek_chl_T    <- fit_rf(parm='Ek',chl=TRUE,full=TRUE)
#fits_alpha     <- fit_rf(parm='alpha',chl=FALSE,full=TRUE)
fits_alpha_chl_T <- fit_rf(parm='alpha',chl=TRUE,full=TRUE)
#fits_PBmax     <- fit_rf(parm='PBmax',chl=FALSE,full=TRUE)
fits_PBmax_chl_T <- fit_rf(parm='PBmax',chl=TRUE,full=TRUE)


#saveRDS(fits_Ek,       'results/fits_Ek.rds')
saveRDS(fits_Ek_chl_T,   'results/fits_Ek_chl_T.rds')
#saveRDS(fits_alpha,    'results/fits_alpha.rds')
saveRDS(fits_alpha_chl_T,'results/fits_alpha_chl_T.rds')
#saveRDS(fits_PBmax,    'results/fits_PBmax.rds')
saveRDS(fits_PBmax_chl_T,'results/fits_PBmax_chl_T.rds')
