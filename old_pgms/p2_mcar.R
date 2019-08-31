library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

pc <- 0.65
m2 <<- 0.1
pt <- pc - m2
nobs <- 100
do_rate <- 0.3
num_n_mi <- 10


x1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 7,
                         FUN= function(x) 
                           
                         {
                           
set.seed(round(1000*do_rate*pc*m2,0) + x)


###################################################
#   fully obnserved data generation and analysis  #
###################################################
                           
dtfull <- dt_p2(n = nobs, pc = pc, pt = pt)


full_mle <- p2_mle(dtfull, m2 = m2)

full_ci <- 
  bind_rows(p2_full_ci(full_mle, "wald", 0.05),
            p2_full_ci(full_mle, "fm", 0.05),
            p2_full_ci(full_mle, "wn", 0.05))%>%
  dplyr::mutate(sim_id = x)%>%
  dplyr::rename(nobs = c_nobs)

  
########################################
# missing data generation and analysis #
########################################

#impose MCAR
dtmiss <- 
  dtfull%>%
  split(.$trt)%>%
  purrr::map_df(.f = function(x){
    x%>%
      dplyr::mutate(r = rbinom(n(), 1, prob = do_rate),
                    y.m = ifelse(r==1 ,NA_integer_ ,y))
  })
  
#check do rate
do_check <- dtmiss%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise(do = mean(r))

#MI
dt_mi <- 
  dtmiss%>%
  split(.$trt)%>%
  purrr::map_df(mi, n_mi = num_n_mi, ym = 'y.m', .id='trt')

#calculate estimate for difference in prorpotions and its variance terms
dt_mi_est <- p2d_mi(dt_mi, m2 = m2)

#combine MI results and calculate CIs using different methods

wald_mi <-
  dt_mi_est%>%
  mi_comb(level=1, phat = 'phat_d', var_phat = 'var_d')%>%
  dplyr::mutate(method = "wald",
                lower_bound = qbar - qnorm(0.975)*sqrt(t),
                upper_bound = qbar + qnorm(0.975)*sqrt(t))

fm_mi <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 'phat_d', var_phat = 'var_dr')%>%
  dplyr::mutate(method = "fm",
                lower_bound = qbar - qnorm(0.975)*sqrt(t),
                upper_bound = qbar + qnorm(0.975)*sqrt(t))

wn_plug_pc <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 'c_phat', var_phat = 'c_phat_var')%>%
  dplyr::mutate(method = "wn-plug for pc",
                lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), nobs = nobs),
                upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), nobs = nobs))

wn_plug_pt <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
  dplyr::mutate(method = "wn-plug for pt",
                lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), nobs = nobs),
                upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), nobs = nobs))

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
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, nobs = nobs),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, nobs = nobs))

wn_mi_pt <- 
  dt_mi_est%>%
  mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
  dplyr::mutate(method = "wn-mi for pt",
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, nobs = nobs),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, nobs = nobs))

wn_mi <- 
  tibble(method = "wn-mi", 
         pc = wn_mi_pc$qbar, lb_pc = wn_mi_pc$lower_bound, ub_pc = wn_mi_pc$upper_bound,
         pt = wn_mi_pt$qbar, lb_pt = wn_mi_pt$lower_bound, ub_pt = wn_mi_pt$upper_bound,
         qbar = wn_mi_pc$qbar - wn_mi_pt$qbar)%>%
  dplyr::mutate(lower_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), lb_wn_p2),
                upper_bound = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), ub_wn_p2))%>%
  dplyr::select(method, qbar, lower_bound, upper_bound)
           
  
mi_all <-
 bind_rows(wald_mi, fm_mi, wn_plug, wn_mi, 
           wn_plug_pc, wn_plug_pt, wn_mi_pc, wn_mi_pt)%>%
  dplyr::mutate(sim_id = x)

out <- list(full_ci, do_check, mi_all)%>%
  purrr::set_names(c("full_ci", "do_check", "mi_all")) 

return(out)
})

p2_eval(x1, m2 = m2)
