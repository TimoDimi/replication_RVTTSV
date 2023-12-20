

RV_PAVG <- function(returns, H=NA){
  # returns: financial log-returns
  # H: Bandwidth for pre-averaging
  
  # Bandwidth parameters
  # NT <- length(t_tick)
  # m <- floor(m_factor * NT/TT)
  if (is.na(H)) { H <- ceiling(sqrt(length(returns)))}
  
  # help functions
  g <- function(x){ x * (1-x) * (x>=0 & x<=1) }
  h_fun <- function(x,y){ (g((x+1)/y) - g(x/y)) }
  g2 = 1/30 # from WolframAlpha with command:   int_0^1 (x * (1-x))^2
  
  # Pre-averaging returns
  r_bar <- rep(NA, length(returns)-H+2)
  for (i in 1:(length(returns)-H+2)){
    r_bar[i] <- sum( g((1:(H-1))/H) * returns[i:(i+H-2)] ) 
  }
  
  # Pre-averaged RV estimator
  sum_h2 <- sum(h_fun(1:(H-1),H)^2) 
  RV <- 1/(H*g2) * sum(r_bar^2)  -  sum_h2/(2*H*g2) * sum(returns^2)
    
  RV
}


