
library(randomForest)


for(i in 1:366){
  X <- data.frame(PAR=t(par[1:i,]), SST=t(sst[1:i,]), CHL=t(chl[1:i,]))
  
  fit <- randomForest(Ek ~ ., data=X)  
  
}