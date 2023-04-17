
Is <- seq(0,800)
P0 <- 3
ns <- 10
alpha <- 0.01
beta  <- 0.01

Pb <- function(I,alpha,P0,beta) P0*(1-exp(-alpha*Is/P0))*exp(-beta*Is/P0) 

fs <- seq(1,2,length.out=ns)  #factors

par(mfrow=c(1,1))
plot(Is,Pb(Is,alpha,P0,beta),type='l',ylim=c(0,0.8))
for(i in 2:ns) lines(Is,Pb(Is,fs[i]*alpha,P0,fs[i]*beta))

#plot(alpha*fs,beta*fs)



