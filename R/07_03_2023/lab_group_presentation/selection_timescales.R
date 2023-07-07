
delta <- c(0.1,0.2,0.5,1)

ts <- seq(0,30,0.1)

r0 <- 1

#pdf()
plot(-999,ylim=c(0,3),xlim=range(ts),xlab='days',ylab='ratio')

for(i in 1:length(delta)){
  c1 <- 
  lines(ts,r0*exp(delta[i]*ts),lty=i)
  lines(ts,r0*exp(-delta[i]*ts),ylim=c(0,3),type='l',lty=i)
}
legend(x=6,y=1.75,legend=c(expression(Delta*mu~'= 0.1'),
                          expression(Delta*mu~'= 0.2'),
                          expression(Delta*mu~'= 0.5'),
                          expression(Delta*mu~'= 1.0')),bty='n',lty=c(1,2,3,4))



par(mfrow=c(1,1))
plot(-999,ylim=c(0,1),xlim=range(ts),xlab='',ylab='')
mtext(side=1,'Days',line=2.5); mtext(side=2,'Proportion high/low light strain',line=2.5)
for(i in 1:length(delta)){
  c1 <- 1/(1+r0*exp(-delta[i]*ts))
  c2 <- 1 - c1
  lines(ts,c1,lty=i)
  lines(ts,c2,lty=i)
}
legend(x=15,y=0.75,legend=c(expression(Delta*mu~'= 0.1'),
                           expression(Delta*mu~'= 0.2'),
                           expression(Delta*mu~'= 0.5'),
                           expression(Delta*mu~'= 1.0')),bty='n',lty=c(1,2,3,4))




c1 <- 1/(1+r0*exp(-delta[3]*ts))
plot(c1,ylim=c(0,1))
lines(1-c1)

c11 <- 0.5*exp(0.7*ts)
c22 <- 0.5*exp(0.2*ts)
plot(c11,ylim=c(0,1))
lines(c22)

plot(c1/(1-c1))

plot(c11/c22)



Is <- 1:500
Pm <- runif(1,1,10)
alpha <- runif(1,0.01,0.1)


plot(Pm*(1-exp(-(alpha/Pm)*Is)),type='l',ylim=c(0,10))
lines(Is,Pm*tanh((alpha/Pm)*Is))
