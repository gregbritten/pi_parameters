

p <- c(u1=1,
       u2=0.2,
       K=1,
       m=0.1,
       S=0.1)
x0 <- 1
T <- 1000
dt <- 0.1
time <- seq(0,0.1*T,length.out=T)

ss <- function(p,dt,T,x0){
  B1 = B2 = N <- numeric(T)
  B1[1] = B2[1] = N[1] <- x0
  
  for(i in 2:T){
    dB1_dt <- p['u1']*B1[i-1]*(N[i-1]/(N[i-1]+K)) - p['m']*B1[i-1]
    dB2_dt <- p['u2']*B2[i-1]*(N[i-1]/(N[i-1]+K)) - p['m']*B2[i-1]
    dN_dt  <- p['S'] - p['u1']*B1[i-1]*(N[i-1]/(N[i-1]+p['K'])) - p['u2']*B2[i-1]*(N[i-1]/(N[i-1]+p['K']))
    
    B1[i] <- B1[i-1] + dB1_dt*dt  
    B2[i] <- B2[i-1] + dB2_dt*dt  
    N[i]  <- N[i-1]  + dN_dt*dt
  }
  return(data.frame(B1=B1,B2=B2))
}

x0 <- 0.5
p['u2'] <- 0.2
  matplot(time,ss(p,dt,T,x0),type='l',lty=1,col='black',ylab='',xlab='Time [days]')
  mtext('Relative Abundance',side=2,line=2.5)
  #matplot(time,ss(p,dt,T,x0)[,2]/ss(p,dt,T,x0)[,1],type='l',lty=1,col='black',ylab='',ylim=c(0,5))
  #matplot(time,ss(p,dt,T,x0)[,1]/ss(p,dt,T,x0)[,2],type='l',lty=1,col='black',ylab='',add=TRUE)
p['u2'] <- 0.5
  matplot(time,ss(p,dt,T,x0),type='l',lty=2,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,2]/ss(p,dt,T,x0)[,1],type='l',lty=1,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,1]/ss(p,dt,T,x0)[,2],type='l',lty=1,col='black',ylab='',add=TRUE)
p['u2'] <- 0.8
  matplot(time,ss(p,dt,T,x0),type='l',lty=3,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,2]/ss(p,dt,T,x0)[,1],type='l',lty=1,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,1]/ss(p,dt,T,x0)[,2],type='l',lty=1,col='black',ylab='',add=TRUE)
p['u2'] <- 0.9
  matplot(time,ss(p,dt,T,x0),type='l',lty=4,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,2]/ss(p,dt,T,x0)[,1],type='l',lty=1,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,1]/ss(p,dt,T,x0)[,2],type='l',lty=1,col='black',ylab='',add=TRUE)
#p['u2'] <- 0.9
  #matplot(time,ss(p,dt,T,x0),type='l',lty=5,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,2]/ss(p,dt,T,x0)[,1],type='l',lty=1,col='black',ylab='',add=TRUE)
  #matplot(time,ss(p,dt,T,x0)[,1]/ss(p,dt,T,x0)[,2],type='l',lty=1,col='black',ylab='',add=TRUE)
legend('right',legend=c(expression(Delta*mu~'= 0.8'),
                        expression(Delta*mu~'= 0.5'),
                        expression(Delta*mu~'= 0.2'),
                        expression(Delta*mu~'= 0.1')),bty='n',lty=c(1,2,3,4),cex=0.8)
  

plot(B2)

plot(time,B2/B1)
plot(time,N)

plot(time,B2)
