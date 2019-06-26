dtfull.p2.create <- function(n, pc, pt, add.x = F){

  dtfull <-
    dplyr::tibble(
      y = rbinom(n = n, 1, prob = pc),
      trt = "c"
    )%>%
    dplyr::bind_rows(dplyr::tibble(
      y = rbinom(n = n, 1, prob = pt),
      trt = "t"
    ))
  
  if (add.x == T){
    
    dtfull <- dtfull%>%
      split(.$trt)%>%
      purrr::map_df(.f = function(x){
        x%>%
          dplyr::mutate(x.strong = case_when(y == 0 ~ rbinom(n = n, 1, prob = 0.6),
                                             y == 1 ~ rbinom(n = n, 1, prob = 0.2)),
                        x.weak = rbinom(n = n, 1, prob = 0.6))
      }
        )
    
  }
  
  return(dtfull)
    
}
