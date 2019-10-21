cca_ch <- function(dt){

  #mean m2 for cca
  m2_ch <- dt%>%
    purrr::map_df(.f=function(x) x$full_ci)%>%
    summarise(mean_m2 = (mean(phat_d)))
  
  #check mean drop-out rate
  do_ch <- dt%>%
    purrr::map_df(.f=function(x) x$do_check)%>%
    dplyr::group_by(trt)%>%
    summarise(mean_do = mean(do))

  out <- list(m2_ch, do_ch)%>%
    purrr::set_names(c('m2_ch', 'do_ch'))
  
  return(out)
  
}
