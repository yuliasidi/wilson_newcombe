library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)


pc <- 0.9
m2 <<- 0.025
pt <- pc - m2
n_obs <- 500
do_rate <- 0.1
num_n_mi <- 10
mp_x1_val <- 0.15
set_n <- 10


x1 <- parallel::mclapply(X = 1:10000, 
                         mc.cores = 10,
                         FUN= function(x) 
                           
                         {
                           
set.seed(12000*set_n + x)


###################################################
#   fully obnserved data generation and analysis  #
###################################################
                           
dtfull <- dt_p2(n = n_obs, pc = pc, pt = pt, add_x = T)%>%
  dplyr::filter(x_desc == "weak")


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

#calculate conditional probabilities forr missingness for x = 0

mp_dt <- expand.grid(trt = c('c', 't'),
                     x_desc = c('weak'))

mp_dt <- mp_dt%>%
  dplyr::mutate(mp_x1 = mp_x1_val)%>%
  dplyr::mutate(mp_x0 = ifelse(trt == 'c', 
                  pmap_dbl(list(mp_x1 = mp_x1, xs_ass = x_desc), mp_x0, do_tar = do_rate, p_y1 = pc),
                  pmap_dbl(list(mp_x1 = mp_x1, xs_ass = x_desc), mp_x0, do_tar = do_rate, p_y1 = pt)))

mp_dt <- mp_dt%>%
  tidyr::gather(key = 'x', 'mp_x', c(mp_x0, mp_x1) )%>%
  dplyr::mutate(x = ifelse(x == 'mp_x0', 0, 1),
                trt = as.character(trt),
                x_desc = as.character(x_desc))

dtmiss <- 
  dtfull%>%
    tidyr::nest(-c(trt, x, x_desc))%>%
    dplyr::left_join(mp_dt, by = c('trt', 'x_desc', 'x'))%>%
    dplyr::mutate(data_m = purrr::pmap(list(data, mp_x), 
                                   .f = function(dt, mp_x){
                                     dt%>%
                                       dplyr::mutate(r = rbinom(n(), 1, prob = mp_x),
                                                      y.m = ifelse(r==1 ,NA_integer_ ,y))
                                   }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()

  
#check do rate
do_check <- dtmiss%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise(do = mean(r))

do_check_x <- dtmiss%>%
  dplyr::group_by(trt, x)%>%
  dplyr::summarise(do = mean(r))

del_seq <- 0
  
#if do rate is zero for one of the groups, delete observations again  
while(sum(dtmiss$r[dtmiss$trt=='c' & dtmiss$x==0]) == 0 ||
      sum(dtmiss$r[dtmiss$trt=='c' & dtmiss$x==1]) == 0 || 
      sum(dtmiss$r[dtmiss$trt=='t' & dtmiss$x==0]) == 0 ||
      sum(dtmiss$r[dtmiss$trt=='t' & dtmiss$x==1]) == 0){

  del_seq <- del_seq + 1
  set.seed(1234*del_seq)
  
dtmiss <- 
  dtfull%>%
    tidyr::nest(-c(trt, x, x_desc))%>%
    dplyr::left_join(mp_dt, by = c('trt', 'x_desc', 'x'))%>%
    dplyr::mutate(data_m = purrr::pmap(list(data, mp_x), 
                                   .f = function(dt, mp_x){
                                     dt%>%
                                       dplyr::mutate(r = rbinom(n(), 1, prob = mp_x),
                                                      y.m = ifelse(r==1 ,NA_integer_ ,y))
                                   }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()

}

do_check <- dtmiss%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise(do = mean(r))%>%
  dplyr::mutate(set_n = set_n)

do_check_x <- dtmiss%>%
  dplyr::group_by(trt, x)%>%
  dplyr::summarise(do = mean(r))


do_check_x <- dtmiss%>%
  dplyr::group_by(trt, x)%>%
  dplyr::summarise(do = mean(r))

#check the the obseved value are not only 1s
check_ymean <- dtmiss%>%
  filter(r==0)%>%
  group_by(trt, x)%>%
  summarise(ymean = mean(y.m))%>%
  dplyr::mutate(check=ymean>0&ymean<1)%>%
  dplyr::pull(check)

if (check_ymean%>%all() & length(check_ymean)==4){

    
  #MI
  dt_mi <- 
    dtmiss%>%
    tidyr::nest(-c(trt, x))%>%
    dplyr::mutate(data_im = purrr::map(data, mi, n_mi = num_n_mi, ym ='y.m', phat_out=F))%>%
    dplyr::select(- data)%>%
    tidyr::unnest()%>%
    tidyr::unnest()%>%
    dplyr::group_by(n, trt)%>%
    dplyr::summarise(phat = mean(y.im), n_obs = dplyr::n())%>%
    dplyr::ungroup()

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
                  lower_bound = pmap_dbl(list(phat = qbar), lb_wn, z = qnorm(0.975), n_obs = n_obs),
                  upper_bound = pmap_dbl(list(phat = qbar), ub_wn, z = qnorm(0.975), n_obs = n_obs))
    
    wn_plug_pt <- 
    dt_mi_est%>%
    mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
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
    mi_comb(level=1, phat = 'c_phat', var_phat = 'c_phat_var')%>%
    dplyr::mutate(method = "wn-mi for pc",
                  lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, n_obs = n_obs),
                  upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, n_obs = n_obs))
    
    wn_mi_pt <- 
    dt_mi_est%>%
    mi_comb(level=1, phat = 't_phat', var_phat = 't_phat_var')%>%
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
                  set_n = set_n)
    
    out <- list(full_ci, do_check, mi_all)%>%
    purrr::set_names(c("full_ci", "do_check", "mi_all")) 
    }

else{ 
  out <- list('simulation was not completed due to phat = 1 in one of the arms')%>%
    purrr::set_names('err')
  
}

return(out)
 
})

saveRDS(x1, sprintf("results/p2_mar_weak_set_n%s.rds",
                    set_n))
