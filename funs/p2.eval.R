p2_eval <- function(dt, m2, mi.level=1){
  
  dt1 <- 
    dt%>%
    purrr::map_df(.f=function(x) x$mi.sum.all,.id = 'sim')%>%
    dplyr::filter(method%in%c("wald", "fm", "wn-mi", "wn-plug"))%>%
    dplyr::mutate(coverage = ifelse(m2<=upper.bound & m2>=lower.bound, 1, 0),
                  ci.length = upper.bound - lower.bound,
                  outrange = ifelse(upper.bound>1 | lower.bound< -1, 1, 0),
                  width0 = ifelse(upper.bound==lower.bound, 1, 0)
    )

  if (mi.level==1){
    dt2 <-
      dt1%>%  
      dplyr::group_by(method)%>%
      dplyr::summarise(mean.qbar = mean(qbar), mean.cov = mean(coverage), mean.length = mean(ci.length), 
                       mean.outrange = mean(outrange), mean.width0 = mean(width0))
    
  }
  
  if (mi.level==2){
    dt2 <-
      dt1%>%  
      dplyr::group_by(method, mu.k, sd.k)%>%
      dplyr::summarise(mean.qbar = mean(qbar), mean.cov = mean(coverage), mean.length = mean(ci.length), 
                       mean.outrange = mean(outrange), mean.width0 = mean(width0))
    
  }
  
  return(dt2)
  
}

