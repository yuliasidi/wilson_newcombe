lb.wn.ign <- function(z, qhat, n, rm){
  (2*qhat + z^2/n + z^2*rm/n)/(2*(1 + z^2/n + z^2*rm/n)) - 
    sqrt(
      (2*qhat + z^2/n + z^2*rm/n)^2/(2*(1 + z^2/n + z^2*rm/n))^2 -
        qhat^2/(1 + z^2/n + z^2*rm/n)
    )
}
