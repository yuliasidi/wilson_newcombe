p1.eval <- function(dt, pc){
  
  dt%>%
    purrr::map_df(.f=function(x) x$mi.sum.all,.id = 'sim')%>%
    dplyr::mutate(coverage = ifelse(pc<=upper.bound & pc>=lower.bound, 1, 0),
                  ci.length = upper.bound - lower.bound,
                  out01 = ifelse(upper.bound>1 | lower.bound<0, 1, 0),
                  width0 = ifelse(upper.bound==lower.bound, 1, 0)
    )%>%
    dplyr::group_by(method, mu.k, sd.k)%>%
    dplyr::summarise(mean.phat = mean(qbar), mean.cov = mean(coverage), 
                     mean.length = mean(ci.length), mean.out01 = mean(out01), mean.width0 = mean(width0))
  
}

