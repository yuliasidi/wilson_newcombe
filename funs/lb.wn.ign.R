lb.wn.ign <- function(z, qhat, nobs, rn){
  (2*qhat + z^2/nobs + z^2*rn/nobs)/(2*(1 + z^2/nobs + z^2*rn/nobs)) - 
    sqrt(
      (2*qhat + z^2/nobs + z^2*rn/nobs)^2/(2*(1 + z^2/nobs + z^2*rn/nobs))^2 -
        qhat^2/(1 + z^2/nobs + z^2*rn/nobs)
    )
}
