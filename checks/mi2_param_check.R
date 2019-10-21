
set <- seq(1,24,1)

tt <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_mnar_params/mi_par_n%s.rds", set_n))
  
  x%>%
    purrr::map_df(.f=function(x) x$mu_k,.id = 'sim')%>%
    summarise_at(.vars = c('mu_kc','mu_kt'), .funs = c(mean, sd))%>%
    dplyr::mutate(set_n = set_n)}, .id = 'set_n' )


tt1 <- tt[,2:6]

tt1 <- tt1%>%mutate(mu_ksim = round((mu_kc_fn1 + mu_kt_fn1)/2, 3),
                    sd_k    = round((mu_kc_fn2 + mu_kt_fn2)/2, 3))%>%
  select(-c(mu_kc_fn1, mu_kt_fn1, mu_kc_fn2, mu_kt_fn2))

saveRDS(tt1, "mnar_pms.rds")
