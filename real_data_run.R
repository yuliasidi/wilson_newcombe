library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

source('funs/neff_li.R')
map_df(set, neff_li, path_res = 'p2_mcar/p2_mcar_set')%>%
  dplyr::mutate(method = 'li')

x <- read.csv("covid19/data/clean-outside-hubei.csv")

x1 <- x %>% 
  dplyr::filter(country == "China", is.na(sex)==F) #chineese patients with reported sex

dtmiss <- x1 %>%
  dplyr::mutate(sym = ifelse(grepl(" ", symptoms) > 0, 
                             1, 
                             ifelse(is.na(symptoms)==T, symptoms, 0))) %>%
  dplyr::mutate(y.m = sym,
                trt = ifelse(sex=="female", 'c', 't')) %>%
  dplyr::select(y.m, trt)
                
dtmiss %>%
  dplyr::count(trt, y.m)

num_n_mi <- 10


dt_mi <- 
  dtmiss%>%
  dplyr::mutate(
    r = ifelse(is.na(y.m)==TRUE, 1, 0)
  )%>%
  tidyr::nest(y.m = -trt)%>%
  dplyr::mutate(y.mi = purrr::map(y.m,function(x){
    mi(x,n_mi = num_n_mi)
  }))%>%
  tidyr::unnest(cols = c(y.mi))
  
#calculate estimate for difference in prorpotions and its variance terms
dt_mi_est <- p2d_mi(dt_mi, m2 = 0.1)

wald_mi <-
  dt_mi_est%>%
  mi_comb(level=1, phat = 'phat_d', var_phat = 'var_d')%>%
  dplyr::mutate(method = "wald",
                lower_bound = qbar - qt(0.975, v)*sqrt(t),
                upper_bound = qbar + qt(0.975, v)*sqrt(t))

wn_plug_pc <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 'c_phat', var_phat = 'c_phat_var')%>%
  dplyr::mutate(method = "wn-plug for pc",
                lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="c"][1]),
                upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="c"][1]))

wn_plug_pt <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
  dplyr::mutate(method = "wn-plug for pt",
                lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="t"][1]),
                upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="t"][1]))

wn_plug <- 
  tibble(method = "wn-plug", 
         pc = wn_plug_pc$qbar, lb_pc = wn_plug_pc$lower_bound, ub_pc = wn_plug_pc$upper_bound,
         pt = wn_plug_pt$qbar, lb_pt = wn_plug_pt$lower_bound, ub_pt = wn_plug_pt$upper_bound,
         qbar = wn_plug_pc$qbar - wn_plug_pt$qbar)%>%
  dplyr::mutate(lower_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), lb_wn_p2),
                upper_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), ub_wn_p2))%>%
  dplyr::select(method, qbar, lower_bound, upper_bound)

wn_mi_pc <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 'c_phat', var_phat = 'c_phat_var')%>%
  dplyr::mutate(method = "wn-mi for pc",
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="c"][1]),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="c"][1]))

wn_mi_pt <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
  dplyr::mutate(method = "wn-mi for pt",
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="t"][1]),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, 
                                       n_obs = dt_mi$n_obs[dt_mi$trt=="t"][1]))

wn_mi <- 
  tibble(method = "wn-mi", 
         pc = wn_mi_pc$qbar, lb_pc = wn_mi_pc$lower_bound, ub_pc = wn_mi_pc$upper_bound,
         pt = wn_mi_pt$qbar, lb_pt = wn_mi_pt$lower_bound, ub_pt = wn_mi_pt$upper_bound,
         qbar = wn_mi_pc$qbar - wn_mi_pt$qbar)%>%
  dplyr::mutate(lower_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), lb_wn_p2),
                upper_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), ub_wn_p2))%>%
  dplyr::select(method, qbar, lower_bound, upper_bound)


mi_all0 <-
  bind_rows(wald_mi, wn_plug, wn_mi, 
            wn_plug_pc, wn_plug_pt, wn_mi_pc, wn_mi_pt) 


xc <- mi_all0%>%
  dplyr::filter(method%in%c('wn-plug for pc'))%>%
  dplyr::mutate(neff = floor(qbar*(1-qbar)/t))%>%
  dplyr::mutate(lb_c = pmap_dbl(list(n_obs = neff, phat = qbar), lb_wn, z = qnorm(0.975)),
                ub_c = pmap_dbl(list(n_obs = neff, phat = qbar), ub_wn, z = qnorm(0.975)))%>%
  dplyr::rename(qbar_c = qbar)%>%
  dplyr::select(qbar_c, lb_c, ub_c)

xt <- mi_all0%>%
  dplyr::filter(method%in%c('wn-plug for pt'))%>%
  dplyr::mutate(neff = floor(qbar*(1-qbar)/t))%>%
  dplyr::mutate(lb_t = pmap_dbl(list(n_obs = neff, phat = qbar), lb_wn, z = qnorm(0.975)),
                ub_t = pmap_dbl(list(n_obs = neff, phat = qbar), ub_wn, z = qnorm(0.975)))%>%
  dplyr::rename(qbar_t = qbar)%>%
  dplyr::select(qbar_t, lb_t, ub_t)

xx <- bind_cols(xc, xt)%>%
  dplyr::mutate(lower_bound = pmap_dbl(list(qbar_c, lb_c, ub_c, qbar_t, lb_t, ub_t), lb_wn_p2),
                upper_bound = pmap_dbl(list(qbar_c, lb_c, ub_c, qbar_t, lb_t, ub_t), ub_wn_p2),
                qbar = qbar_c - qbar_t)%>%
  dplyr::mutate(method = 'MI-Li') %>%
  dplyr::select(method, qbar, lower_bound, upper_bound)

res <- mi_all0 %>% 
  dplyr::select(method, qbar, lower_bound, upper_bound) %>%
  bind_rows(xx) %>%
  dplyr::mutate(width = upper_bound - lower_bound) %>%
  dplyr::filter(method %in% c('wald', 'wn-plug', 'wn-mi', 'MI-Li'))

print(xtable::xtable(res, digits = c(0,0,3,3,3,3)), include.rownames = F)
knitr::kable(res)

wald_cca <- dtmiss %>%
  dplyr::count(trt, y.m) %>%
  dplyr::filter(is.na(y.m) == FALSE) %>%
  dplyr::group_by(trt) %>%
  dplyr::mutate(p = n/sum(n),
         ntotal = sum(n),
         v = p*(1-p)/ntotal) %>%
  dplyr::filter(y.m==1)



  
  
