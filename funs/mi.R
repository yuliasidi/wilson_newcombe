mi <- function(dt, n.mi, m.mi=0, mu.k=1, sd.k=0){

  #count number of successes and failure to be used in the posterior distribution of p
  ym.sum <- sf.count(dt)
  
  #run the subject level inputation n.mi times
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
  

  #if m=0 then determine imputed values of y and combine the result
  if (m.mi==0){
    
    t1 <-
      t%>%
      dplyr::mutate(dt.mi.sum = purrr::map(dt.mi, .f = function(x){
        x%>%
          dplyr::mutate(y.im = case_when(is.na(y.m)==T ~ ifelse(pstar>=p.thresh, 1, 0),
                                         is.na(y.m)==F ~ as.numeric(y.m)))%>%
          dplyr::summarise(phat = mean(y.im), nobs = n())
        }))%>%
      dplyr::select(n, dt.mi.sum)%>%
      tidyr::unnest()
    
  }

  #if m>0 then the two-stage imputation is employed
  if (m.mi>0){

    k.mults <- tibble(m = seq(1, m.mi,1),
                      k = rnorm(m.mi, mu.k, sd.k))
   
    # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
   t1 <-
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
    
    
  }  
  
return(t1)
  
}
