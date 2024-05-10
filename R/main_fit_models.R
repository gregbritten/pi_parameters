library(randomForest)
source('r/main_process_nc.R')
source('r/f_fit_rf.r')

fit_Ek_all     <- fit_rf(D=D_nc, parm='Ek',   ntime=365,regions=regions,depth='all')
fit_alpha_all  <- fit_rf(D=D_nc, parm='alpha',ntime=365,regions=regions,depth='all')
fit_PBmax_all  <- fit_rf(D=D_nc, parm='PBmax',ntime=365,regions=regions,depth='all')

saveRDS(fit_Ek_all,   'results/fit_Ek_all.rds')
saveRDS(fit_alpha_all,'results/fit_alpha_all.rds')
saveRDS(fit_PBmax_all,'results/fit_PBmax_all.rds')


fit_Ek_40    <- fit_rf(D=D_nc, parm='Ek',   ntime=365,regions=regions,depth='40')
fit_alpha_40 <- fit_rf(D=D_nc, parm='alpha',ntime=365,regions=regions,depth='40')
fit_PBmax_40 <- fit_rf(D=D_nc, parm='PBmax',ntime=365,regions=regions,depth='40')

saveRDS(fit_Ek_40,   'results/fit_Ek_40.rds')
saveRDS(fit_alpha_40,'results/fit_alpha_40.rds')
saveRDS(fit_PBmax_40,'results/fit_PBmax_40.rds')

