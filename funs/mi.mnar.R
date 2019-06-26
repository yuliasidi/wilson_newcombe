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

  t <- tibble(n = seq(1,n.mi,1))%>%
   dplyr::mutate(dt.mi = map(n, .f = function(x){
     
     dt%>%
       dplyr::filter(is.na(y.m) == T)%>%
       dplyr::mutate(
         p.thresh = runif(n(), 0, 1),
         pstar = rbeta(n(), shape1 = ym.sum$success - 1, shape2 = ym.sum$fail - 1))%>%
       dplyr::select(p.thresh, pstar)%>%
       dplyr::bind_rows(dt%>%
                          dplyr::filter(is.na(y.m) == F)%>%
                          dplyr::select(y.m))
   }))
  

  k.mults <- tibble(m = seq(1, m.mi,1),
                    k = rnorm(m.mi, mu.k, sd.k))
  

  # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
  nested.mi.res <-
    k.mults%>%
    dplyr::mutate(res = purrr::map(k, .f = function(k){
      
      t%>%
        unnest()%>%
        dplyr::mutate(pstar.k = k * pstar)%>%
        dplyr::mutate(y.im = case_when(is.na(y.m)==T ~ ifelse(pstar.k>=p.thresh, 1, 0),
                                       is.na(y.m)==F ~ as.numeric(y.m)))%>%
        dplyr::group_by(n)%>%
        dplyr::summarise(phat = mean(y.im), nobs = n())

        }))%>%
    unnest()

  sum.by.m <-
    nested.mi.res%>%
    dplyr::mutate(var.phat = phat*(1 - phat)/n)%>%
    dplyr::group_by(m)%>%
    dplyr::summarise(qbar.m = mean(phat),
                     q.m.var = var(phat),
                     ubar.m = mean(var.phat),
                     n = n())
  
  mi.sum <- 
    sum.by.m%>%
    dplyr::summarise(qbar = mean(qbar.m),
                     ubar = mean(ubar.m),
                     b = var(qbar.m),
                     w = mean(q.m.var),
                     n = mean(n),
                     m = n())
  
  return(mi.sum)
}
