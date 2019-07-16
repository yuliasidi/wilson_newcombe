do_ch <- function(dt){

  #check mean drop-out rate
  dt%>%
    purrr::map_df(.f=function(x) x$do_check)%>%
    dplyr::group_by(trt)%>%
    summarise(mean_do = mean(do))

}
