efold <- function(alpha){
  t    <- seq(0,60,0.001)
  cors <- alpha^t
  e    <- (cors - (1/exp(1)))^2
  return(t[e==min(e)][1])
}

