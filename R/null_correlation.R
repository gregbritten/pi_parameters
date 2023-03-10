
Is <- seq(0,100)
PIcurve <- function(a,Pm) Pm*tanh(a*Is/Pm)

niter <- 10000
pars <- data.frame(a=numeric(),Pm=numeric())

for(i in 1:niter){
  a  <- runif(1,0,0.2)
  Pm <- runif(1,5,10)
  PI <- Pm*tanh(a*Is/Pm) + rnorm(n=101,sd=0.2)
  fit <- try(nls(PI ~ PIcurve(a,Pm), start=list(a=0.1,Pm=10)),silent=TRUE)
  if(class(fit)!='try-error'){
    pars <- pars %>% add_row(a=coefficients(fit)[1],Pm=coefficients(fit)[2])
  }
}

plot(Is,PI)

fit <- 
c(a,Pm)



a  <- runif(1,0,0.2)
Pm <- runif(1,5,10)
PI <- Pm*tanh(a*Is/Pm) + rnorm(n=101,sd=0.2)

plot(Is,6*tanh(1*Is/8),ylim=c(0,10))
