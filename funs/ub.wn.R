ub.wn <- function(z, phat, nobs){

  (phat + z^2/(2*nobs) + 
    z*sqrt(
      phat*(1 - phat)/nobs + (z/(2*nobs))^2
      ))/
    (1 + z^2/nobs)
  
}