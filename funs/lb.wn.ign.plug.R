lb.wn.ign.plug <- function(z, qhat, n){

  (qhat + z^2/(2*n) - 
    z*sqrt(
      qhat*(1 - qhat)/n + (z/(2*n))^2
      ))/
    (1 + z^2/n)
  
}