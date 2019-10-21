library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

pc <- 0.65
m2 <<- 0.025
pt <- pc - m2
n_obs <- 50
do_rate <- 0.3
num_n_mi <- 2
num_m_mi <- 100
set_n <- 21 

mp_y1 <- 0.42

sd_k <- 0.1


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

  mu_k <- tibble(
  mp_c = mean(dtmiss$r[dtmiss$trt=="c"]), #P(R=1)
  mp_t = mean(dtmiss$r[dtmiss$trt=="t"]),
  mp_y1_c = mean(dtmiss$r[dtmiss$trt=="c" & dtmiss$y==1]), #P(R=1|Y=1)
  mp_y1_t = mean(dtmiss$r[dtmiss$trt=="t" & dtmiss$y==1]),
  
  mu_kc = (1 - mp_c)/(mp_c * (1 - mp_y1_c)) - (1 - mp_c)/mp_c,
  mu_kt = (1 - mp_t)/(mp_t * (1 - mp_y1_t)) - (1 - mp_t)/mp_t)

   out <- list(mu_k)%>%
     purrr::set_names(c("mu_k")) 
 }
 
 else{ 
   out <- list('simulation was not completed due to phat = 1 in one of the arms')%>%
     purrr::set_names('err')
   
 }
 
 return(out)
})

mi_par <- 
  x1%>%
  purrr::map_df(.f=function(x) x$mu_k,.id = 'sim')%>%
  summarise_at(.vars = c('mu_kc','mu_kt'), .funs = c(mean, sd))%>%
  dplyr::mutate(set_n = set_n)
  
saveRDS(x1, sprintf("results/mi_par_n%s.rds",
                     set_n))
