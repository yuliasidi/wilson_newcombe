library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

pc <- 0.65
m2 <<- 0.1
pt <- pc - m2
n_obs <- 100
do_rate <- 0.3
num_n_mi <- 2
num_m_mi <- 100
set_n <- 7 

mp_y1 <- 0.42

mu_k <- 1.2
sd_k <- 0.08


x1 <- parallel::mclapply(X = 1:10000, 
                         mc.cores = 24,
                         FUN= function(x) 
                           
                         {
                           
set.seed(13000*set_n + x)
 
 
###################################################
#   fully obnserved data generation and analysis  #
###################################################

 dtfull <- dt_p2(n = n_obs, pc = pc, pt = pt)
 
 
 full_mle <- p2_mle(dtfull, m2 = m2)
 
 full_ci <- 
   bind_rows(p2_full_ci(full_mle, "wald", 0.05),
             p2_full_ci(full_mle, "fm", 0.05),
             p2_full_ci(full_mle, "wn", 0.05))%>%
   dplyr::mutate(sim_id = x,
                 set_n = set_n)%>%
   dplyr::rename(n_obs = c_n_obs)
 
 
 ########################################
 # missing data generation and analysis #
 ########################################
 
 #impose MNAR
 dtmiss <- 
   dtfull%>%
   tidyr::nest(-trt)%>%
   dplyr::mutate(mp_y1_val = mp_y1,
                 mp_y0_val = ifelse(trt == 'c',
                                mp_y0(do_tar = do_rate, mp_y1 = mp_y1, p_y1 = pc),
                                mp_y0(do_tar = do_rate, mp_y1 = mp_y1, p_y1 = pt)))%>%
   dplyr::mutate(data_m = purrr::pmap(list(data, mp_y1_val, mp_y0_val),
                                      .f = function(x, mp_y1_val, mp_y0_val){
     x%>%
       dplyr::filter(y == 1)%>%
       dplyr::mutate(r = rbinom(n(), 1, mp_y1_val))%>%
       dplyr::bind_rows(
         x%>%
           dplyr::filter(y == 0)%>%
           dplyr::mutate(r = rbinom(n(), 1, mp_y0_val)))%>%
       dplyr::mutate(
         y.m = ifelse(r==1 ,NA_integer_ ,y)
       )
     
   }))%>%
   dplyr::select(-c(data, mp_y1_val, mp_y0_val))%>%
   tidyr::unnest()
  
 #check do rate
 do_check <- dtmiss%>%
   dplyr::group_by(trt)%>%
   dplyr::summarise(do = mean(r))%>%
   dplyr::mutate(set_n = set_n)
 
 del_seq <- 0
 
 #if do rate is zero for one of the groups, delete observations again  
 while(sum(dtmiss$r[dtmiss$trt=='c']) == 0 || sum(dtmiss$r[dtmiss$trt=='t']) == 0){
   
   del_seq <- del_seq + 1
   set.seed(1234*del_seq)
   
   dtmiss <- 
     dtfull%>%
     tidyr::nest(-trt)%>%
     dplyr::mutate(mp_y1_val = mp_y1,
                   mp_y0_val = ifelse(trt == 'c',
                                      mp_y0(do_tar = do_rate, mp_y1 = mp_y1, p_y1 = pc),
                                      mp_y0(do_tar = do_rate, mp_y1 = mp_y1, p_y1 = pt)))%>%
     dplyr::mutate(data_m = purrr::pmap(list(data, mp_y1_val, mp_y0_val),
                                        .f = function(x, mp_y1_val, mp_y0_val){
                                          x%>%
                                            dplyr::filter(y == 1)%>%
                                            dplyr::mutate(r = rbinom(n(), 1, mp_y1_val))%>%
                                            dplyr::bind_rows(
                                              x%>%
                                                dplyr::filter(y == 0)%>%
                                                dplyr::mutate(r = rbinom(n(), 1, mp_y0_val)))%>%
                                            dplyr::mutate(
                                              y.m = ifelse(r==1 ,NA_integer_ ,y)
                                            )
                                          
                                        }))%>%
     dplyr::select(-c(data, mp_y1_val, mp_y0_val))%>%
     tidyr::unnest()
   
   
 }
 
 check_ymean <- dtmiss%>%
  filter(r==0)%>%
  group_by(trt)%>%
  summarise(ymean = mean(y.m))%>%
  dplyr::mutate(check=ymean>0&ymean<1)%>%
  dplyr::pull(check)

if (check_ymean%>%all()){
  
   #MI
   dt_mi <- 
     dtmiss%>%
     split(.$trt)%>%
     purrr::map_df(mi, n_mi = num_n_mi, m_mi = num_m_mi, mu_k = mu_k, sd_k = sd_k, ym = 'y.m', .id='trt')
   
   #calculate estimate for difference in prorpotions and its variance terms
   dt_mi_est <- p2d_mi(dt_mi, m2 = m2)
   
   #combine MI results and calculate CIs using different methods
   
   wald_mi <-
     dt_mi_est%>%
     mi_comb(level=2, phat = 'phat_d', var_phat = 'var_d')%>%
     dplyr::mutate(method = "wald",
                   lower_bound = qbar - qnorm(0.975)*sqrt(t),
                   upper_bound = qbar + qnorm(0.975)*sqrt(t))
   
   fm_mi <- 
     dt_mi_est%>%
     mi_comb(level=2, phat = 'phat_d', var_phat = 'var_dr')%>%
     dplyr::mutate(method = "fm",
                   lower_bound = qbar - qnorm(0.975)*sqrt(t),
                   upper_bound = qbar + qnorm(0.975)*sqrt(t))
   
   wn_plug_pc <- 
     dt_mi_est%>%
     mi_comb(level=2, phat = 'c_phat', var_phat = 'c_phat_var')%>%
     dplyr::mutate(method = "wn-plug for pc",
                   lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), n_obs = n_obs),
                   upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), n_obs = n_obs))
   
   wn_plug_pt <- 
     dt_mi_est%>%
     mi_comb(level=2, phat = 't_phat', var_phat = 't_phat_var')%>%
     dplyr::mutate(method = "wn-plug for pt",
                   lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), n_obs = n_obs),
                   upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), n_obs = n_obs))
   
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
     mi_comb(level=2, phat = 'c_phat', var_phat = 'c_phat_var')%>%
     dplyr::mutate(method = "wn-mi for pc",
                   lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, n_obs = n_obs),
                   upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, n_obs = n_obs))
   
   wn_mi_pt <- 
     dt_mi_est%>%
     mi_comb(level=2, phat = 't_phat', var_phat = 't_phat_var')%>%
     dplyr::mutate(method = "wn-mi for pt",
                   lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, n_obs = n_obs),
                   upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, n_obs = n_obs))
   
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
     dplyr::mutate(sim_id = x,
                   set_n = set_n,
                   mu_k = mu_k,
                   sd_k = sd_k)
   
   out <- list(full_ci, do_check, mi_all)%>%
     purrr::set_names(c("full_ci", "do_check", "mi_all")) 
 }
 
 else{ 
   out <- list('simulation was not completed due to phat = 1 in one of the arms')%>%
     purrr::set_names('err')
   
 }
 
 return(out)
})

saveRDS(x1, sprintf("results/p2_mnar_set_n%s.rds",
                    set_n))
