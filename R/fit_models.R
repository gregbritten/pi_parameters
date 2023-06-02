source('r/process_csvs.r') ##--data processing--###########

fits_Ek        <- fit_rf(parm='Ek',chl=FALSE)
fits_Ek_chl    <- fit_rf(parm='Ek',chl=TRUE)
fits_alpha     <- fit_rf(parm='alpha',chl=FALSE)
fits_alpha_chl <- fit_rf(parm='alpha',chl=TRUE)
fits_PBmax     <- fit_rf(parm='PBmax',chl=FALSE)
fits_PBmax_chl <- fit_rf(parm='PBmax',chl=TRUE)

saveRDS(fits_Ek,       'results/fits_Ek.rds')
saveRDS(fits_Ek_chl,   'results/fits_Ek_chl.rds')
saveRDS(fits_alpha,    'results/fits_alpha.rds')
saveRDS(fits_alpha_chl,'results/fits_alpha_chl.rds')
saveRDS(fits_PBmax,    'results/fits_PBmax.rds')
saveRDS(fits_PBmax_chl,'results/fits_PBmax_chl.rds')
