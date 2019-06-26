p.rmle.fm <- function(M2, n_T, n_C, p_C, p_T){
   
    theta <- n_T/n_C
    a <- 1 + theta
    b <- -1*(1 + theta + p_C + theta*p_T + M2*(2 + theta)) 
    c <- M2^2 + M2*(2*p_C + theta + 1) + p_C + theta*p_T
    d <- -p_C*M2*(1 + M2)
    v <- (b/(3*a))^3 - b*c/(6*a^2) + d/(2*a)
    u <- sign(v)*sqrt((b/(3*a))^2 - c/(3*a))
    u = ifelse(u!=0,u,0.00001)
    w <- (pi+acos(v/(u^3)))/3
    p_C.rmle <- 2*u*cos(w) - b/(3*a)

    return(p_C.rmle)
    
}


