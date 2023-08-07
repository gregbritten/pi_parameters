library(randomForest)
source('r/main_process_nc.R')
source('r/f_fit_rf.r')

fits_Ek_chl_F_365    <- fit_rf(D=D_nc, parm='Ek',   chl=TRUE,full=FALSE,ntime=365,regions=regions)
fits_alpha_chl_F_365 <- fit_rf(D=D_nc, parm='alpha',chl=TRUE,full=FALSE,ntime=365,regions=regions)
fits_PBmax_chl_F_365 <- fit_rf(D=D_nc, parm='PBmax',chl=TRUE,full=FALSE,ntime=365,regions=regions)

saveRDS(fits_Ek_chl_F_365,   'results/fits_Ek_chl_F_365.rds')
saveRDS(fits_alpha_chl_F_365,'results/fits_alpha_chl_F_365.rds')
saveRDS(fits_PBmax_chl_F_365,'results/fits_PBmax_chl_F_365.rds')



