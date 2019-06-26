ub.wn.nonign <- function(z, qhat, nobs, rn, sn){
  (2*qhat + z^2/nobs + z^2*rn/nobs + z^2*sn/nobs)/(2*(1 + z^2/nobs + z^2*rn/nobs + z^2*sn/nobs)) + 
    sqrt(
      (2*qhat + z^2/nobs + z^2*rn/nobs + z^2*sn/nobs)^2/(2*(1 + z^2/nobs + z^2*rn/nobs + z^2*sn/nobs))^2 -
        qhat^2/(1 + z^2/nobs + z^2*rn/nobs + z^2*sn/nobs)
    )
}