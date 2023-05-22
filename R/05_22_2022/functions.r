##--INTERPOLATION--#######################
resize_bilinear <- function(xin,yin,xout,yout,z){
  library(fields)
  obj   <- list(x=1:xin, y=1:yin, z = z)
  tempx <- seq(1,xin,length.out=xout)
  tempy <- seq(1,yin,length.out=yout)
  loc   <- make.surface.grid(list(tempx,tempy))
  look  <- interp.surface(obj,loc)
  return(as.surface(loc,look)$z)
}

##--TEMPERATURE DEPENDENCE CURVES--#########################
tT  <- function(TEMP, a, b, z, w) a*exp(b*TEMP)*(1-((TEMP-z)/w)^2)
#tT2 <- function(TEMP, z, w) 2.5*exp(0.0631*TEMP)*(1-((TEMP-z)/w)^2)  #try fixing a parameter to only estimate two
#tT3 <- function(TEMP, a, z, w) a*exp(0.0631*TEMP)*(1-((TEMP-z)/w)^2) #only estimate three

tT_behren <- function(TEMP) -(3.27E-8)*TEMP^7 + (3.4132E-6)*TEMP^6 - (1.348E-4)*TEMP^5 + (2.462E-3)*TEMP^4 - (0.0205)*TEMP^3 + 0.0617*TEMP^2 + 0.2749*TEMP + 1.2956  
tT_opt    <- function(a,b,z,w) (b*z + sqrt(b^2 * w^2 + 1) - 1)/b
tT_max    <- function(a,b,z,w) 2*a*exp(b*z + sqrt(b^2*w^2 + 1) - 1)*(sqrt(b^2*w^2 + 1) - 1)/(b^2*w^2) #to comput maximum



