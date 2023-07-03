library(mgcv)
library(randomForest)
library(doParallel)
ncores <- detectCores()      
registerDoParallel(ncores-1) #leave one core for other tasks


source('r/process_csvs.r') ##--data processing--###########

fit_lm <- function(data,chl,rr){
  foreach(i=1:30)%dopar%{
    d     <- D[[i]] %>% filter(complete.cases(sst,par,month,longhurst,basin,biome,daylength,E_surf),
                                              region==rr)
    if(chl==TRUE) d <- d    %>% filter(complete.cases(chl,kd_490,Zeu,Ez))

    ##-Fit models--############    
    if(chl==FALSE){
      fit      <- lm(Ek ~ sst + par + as.factor(month) + daylength + E_surf, 
                               data=d)}
    if(chl==TRUE){
      fit      <- lm(Ek ~ I(log10(chl)) + kd_490 + Zeu + Ez + sst + par + as.factor(month) + daylength + E_surf, 
                               data=d)}
    #list(summary(fit)$coefficients[,1],cor(test$PBmax,as.numeric(predict(fit,newdata=test))))
    list(summary(fit)$coefficients[,1],summary(fit)$r.squared)
  }
}

