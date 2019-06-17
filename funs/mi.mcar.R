mi.mcar <- function(n.mi, dt){
  
  
  ym.sum <-
    dt%>%
    dplyr::filter(is.na(y.m) == F)%>%
    dplyr::group_by(y.m)%>%
    dplyr::summarise(nobs = n())%>%
    dplyr::mutate(y.name = ifelse(y.m==0, "fail", "success"))%>%
    dplyr::select(-y.m)%>%
    tidyr::spread('y.name', 'nobs')
  
  set.seed(111*n.mi)
  
  dt%>%
    dplyr::filter(is.na(y.m) == T)%>%
    dplyr::mutate(
      p.thresh = runif(n(), 0, 1),
      pstar = rbeta(n(), shape1 = ym.sum$success - 1, shape2 = ym.sum$fail - 1),
      y.im = ifelse(pstar>=p.thresh, 1, 0)
    )%>%
    dplyr::select(y.im)%>%
    dplyr::bind_rows(dt%>%
                       dplyr::filter(is.na(y.m) == F)%>%
                       dplyr::mutate(y.im = y)%>%
                       dplyr::select(y.im))%>%
    dplyr::summarise(phat = mean(y.im), nobs = n())
  
}
