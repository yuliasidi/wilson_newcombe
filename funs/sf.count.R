#count number of successes and failures for variable y.m in the dataframe
sf.count <- function(dt){
  out <- 
    dt%>%
    dplyr::filter(is.na(y.m) == F)%>%
    dplyr::group_by(y.m)%>%
    dplyr::summarise(nobs = n())%>%
    dplyr::mutate(y.name = ifelse(y.m==0, "fail", "success"))%>%
    dplyr::select(-y.m)%>%
    tidyr::spread('y.name', 'nobs')
  
  return(out)
}
