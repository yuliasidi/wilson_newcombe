mi.mnar <- function(n.mi, m.mi, dt, mu.k, sd.k){
  
  #count number of successes and failures
  ym.sum <-
    dt%>%
    dplyr::filter(is.na(y.m) == F)%>%
    dplyr::group_by(y.m)%>%
    dplyr::summarise(nobs = n())%>%
    dplyr::mutate(y.name = ifelse(y.m==0, "fail", "success"))%>%
    dplyr::select(-y.m)%>%
    tidyr::spread('y.name', 'nobs')
  
  set.seed(222*n.mi)
  
  #divide data into complete/incomplete
  dt.incomplete <- dt%>%
    dplyr::filter(is.na(y.m) == T)%>%
    dplyr::mutate(
      p.thresh = runif(n(), 0, 1),
      pstar = rbeta(n(), shape1 = ym.sum$success - 1, shape2 = ym.sum$fail - 1))

  dt.complete <- dt%>%
    dplyr::filter(is.na(y.m) == F)%>%
    dplyr::mutate(y.im = y)%>%
    dplyr::select(y.im)
  
  k.mults <- tibble(m = seq(1, m.mi,1),
                    k = rnorm(m.mi, mu.k, sd.k))
  
  
  # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
  nested.mi.res <-
    k.mults%>%
    dplyr::mutate(res = purrr::map(k, .f = function(k){
      
        dt.incomplete%>%
        dplyr::mutate(pstar.k = k * pstar,
                      y.im = ifelse(pstar.k>=p.thresh, 1, 0))%>%
        dplyr::select(pstar, pstar.k, y.im)%>%
        dplyr::bind_rows(dt.complete)%>%
        dplyr::summarise(phat = mean(y.im), nobs = n())
    }))%>%
    unnest()
      
}
