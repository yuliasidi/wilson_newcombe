p1_eval <- function(dt, trt, ptrt) {
  dt%>%
    purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method%in%c(sprintf('wn-mi for p%s', trt))%>%
    dplyr::mutate(coverage = ifelse(ptrt<=upper_bound & ptrt>=lower_bound, 1, 0))%>%
    summarise(mean_cov = mean(coverage), mean_qbar = mean(qbar))
}