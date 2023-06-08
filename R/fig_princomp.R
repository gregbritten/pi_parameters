
par(mfrow=c(1,1))
plot(NA,xlim=c(0,30),ylim=c(0,1))
cols <- turbo(5)
for(j in 1:length(regions)){
  lines(unlist(lapply(1:30,function(i){
    d <- D[[i]] %>% filter(region==regions[j], complete.cases(chl,par,sst))
    pc   <- princomp(cbind(d$par,d$sst,d$chl))
    eigs <- pc$sdev^2
    perc <- eigs[1]/sum(eigs)
    perc
  })),col=cols[j])}
legend('bottomright',legend=region_long,lty=1, col=cols,bty='n',cex=1)

  
