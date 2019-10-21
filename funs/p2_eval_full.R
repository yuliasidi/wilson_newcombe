p2_eval_full <- function(dt, m2){
  
  dt1 <-
    dt%>%
    purrr::map_df(.f=function(x) x$full_ci,.id = 'sim')%>%
    dplyr::filter(method%in%c("wald", "fm", "wn"))%>%
    dplyr::mutate(coverage = ifelse(m2<=upper_bound & m2>=lower_bound, 1, 0),
                  ci_length = upper_bound - lower_bound,
                  outrange = ifelse(upper_bound>1 | lower_bound< -1, 1, 0),
                  width0 = ifelse(upper_bound==lower_bound, 1, 0)
    )
  
    dt2 <-
      dt1%>%
      dplyr::group_by(method)%>%
      dplyr::summarise(mean_phat = mean(phat_d), mean_cov = mean(coverage), mean_length = mean(ci_length),
                       mean_outrange = mean(outrange), mean_width0 = mean(width0))
      return(dt2)
  
}
