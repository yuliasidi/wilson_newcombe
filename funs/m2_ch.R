m2_ch <- function(dt){
  
  #mean m2 for cca
  dt%>%
    purrr::map_df(.f=function(x) x$full_ci)%>%
    summarise(mean_m2 = (mean(phat_d)))
}
