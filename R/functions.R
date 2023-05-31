
fit_rf <- function(mean_files,chl){
  results <- foreach(i=1:length(mean_files)) %dopar% {
    print(i)
    file <- mean_files[i]
    
    d     <- read.csv(file) %>% filter(complete.cases(sst,par,month,longhurst,basin,biome,daylength,E_surf),
                                       lat>30 & lat<80 & lon < -30 & lon > -90)
    if(chl==TRUE) d <- d              %>% filter(complete.cases(chl,kd_490,Zeu,Ez))
    
    #samp     <- sample(1:nrow(d),size=nrow(d)/5)
    samp     <- sample(1:nrow(d),size=1)
    train    <- d[-samp,] 
    test     <- d[samp,]
    
    if(chl==FALSE){
      fit      <- randomForest(PBmax ~ sst + par + month + longhurst + basin + biome + daylength + E_surf, 
                               data=train, importance=TRUE)
    }
    if(chl==TRUE){
      fit      <- randomForest(PBmax ~ chl + kd_490 + Zeu + Ez + sst + par + month + longhurst + basin + biome + daylength + E_surf, 
                               data=train, importance=TRUE)
    }
    #list(fit$importance, cor(test$PBmax,as.numeric(predict(fit,newdata=test))))
    list(fit$importance, cor(train$PBmax,as.numeric(predict(fit,newdata=train))))
  }
}







  