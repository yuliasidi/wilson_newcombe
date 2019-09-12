neff_li <- function(set_n, path_res, kalt=''){
  
  x0 <- readRDS(sprintf("results/%s_n%s%s.rds",path_res, set_n, kalt))
  x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
  
  xc <- x%>%
    purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method%in%c('wn-plug for pc'))%>%
    dplyr::mutate(neff = floor(qbar*(1-qbar)/t))%>%
    dplyr::mutate(lb_c = pmap_dbl(list(n_obs = neff, phat = qbar), lb_wn, z = qnorm(0.975)),
                  ub_c = pmap_dbl(list(n_obs = neff, phat = qbar), ub_wn, z = qnorm(0.975)))%>%
    dplyr::rename(qbar_c = qbar)%>%
    dplyr::select(sim, qbar_c, lb_c, ub_c)
  
  xt <- x%>%
    purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method%in%c('wn-plug for pt'))%>%
    dplyr::mutate(neff = floor(qbar*(1-qbar)/t))%>%
    dplyr::mutate(lb_t = pmap_dbl(list(n_obs = neff, phat = qbar), lb_wn, z = qnorm(0.975)),
                  ub_t = pmap_dbl(list(n_obs = neff, phat = qbar), ub_wn, z = qnorm(0.975)))%>%
    dplyr::rename(qbar_t = qbar)%>%
    dplyr::select(sim, qbar_t, lb_t, ub_t)
  
  xx <- inner_join(xc, xt, by = "sim")%>%
    dplyr::mutate(lower_bound = pmap_dbl(list(qbar_c, lb_c, ub_c, qbar_t, lb_t, ub_t), lb_wn_p2),
                  upper_bound = pmap_dbl(list(qbar_c, lb_c, ub_c, qbar_t, lb_t, ub_t), ub_wn_p2),
                  qbar = qbar_c - qbar_t,
                  m2 = setting$m2[setting$set_n==set_n])%>%
    dplyr::mutate(coverage = ifelse(m2<=upper_bound & m2>=lower_bound, 1, 0),
                  ci_length = upper_bound - lower_bound,
                  outrange = ifelse(upper_bound>1 | lower_bound< -1, 1, 0),
                  width0 = ifelse(upper_bound==lower_bound, 1, 0))
  out <- xx%>%
    dplyr::summarise(mean_qbar = mean(qbar), mean_cov = mean(coverage), mean_length = mean(ci_length),
                     mean_outrange = mean(outrange), mean_width0 = mean(width0))%>%
    dplyr::mutate(set_n = set_n)%>%
    dplyr::left_join(setting, by = 'set_n')
  
  return(out)
  
}
