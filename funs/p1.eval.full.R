p1.eval.full <- function(dt, pc){
  
  dt%>%
    purrr::map_df(.f=function(x) x$full.sum.all,.id = 'sim')%>%
    dplyr::mutate(coverage = ifelse(pc<=upper.bound & pc>=lower.bound, 1, 0),
                  ci.length = upper.bound - lower.bound,
                  out01 = ifelse(upper.bound>1 | lower.bound<0, 1, 0),
                  width0 = ifelse(upper.bound==lower.bound, 1, 0)
    )%>%
    dplyr::group_by(method)%>%
    dplyr::summarise(mean.phat = mean(phat), mean.cov = mean(coverage), 
                     mean.length = mean(ci.length), mean.out01 = mean(out01), mean.width0 = mean(width0))
  
}

