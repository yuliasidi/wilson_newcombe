ub.wn.nonign <- function(z, qhat, n, b, w, M, N){
 (qhat + z^2/(2*n) + sqrt( qhat*z^2/n + (z/(2*n))^2 + z^2*((1 + 1/M)*b + (1 - 1/N)*w)
                           )
 )/(1 + z^2/n)
}
