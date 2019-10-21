cca_ch <- function(dt){

  #mean phat for cca
  dt%>%
    purrr::map_df(.f=function(x) x$full_ci)%>%
    summarise(mean_m2 = (mean(phat_d)))
  
  #check mean drop-out rate
  dt%>%
    purrr::map_df(.f=function(x) x$do_check)%>%
    dplyr::group_by(trt)%>%
    
    summarise(mean_do = mean(do))%>%
    dplyr::mutate(phat.cca = phat.cca)
  
  
}
