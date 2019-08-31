library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(bin2mi)

source("funs/m2_ch.R")
source("funs/do_ch.R")
source("funs/p2_eval_full.R")


setting <- readRDS('setting.rds')
set <- seq(1,16,1)


p2_full <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_mcar/p2_mcar_set_n%s.rds", set_n))
  
  x1 <- p2_eval_full(x, m2 = setting$m2[setting$set_n==set_n])
  x2 <- x1%>%
    dplyr::mutate(set_n = set_n, 
                  n_exc = purrr::keep(x, .p=function(x) is.character(x[[1]]))%>%
                    length()%>%
                    as.numeric())
})%>%
  left_join(setting, by = 'set_n')%>%
  filter(do_rate==0.1)


#p2_full%>%select(method, mean_cov, set_n)%>%spread(key = "method", value = "mean_cov")
# p2_full%>%
#   ggplot(aes(x=mean_cov,y=method)) +
#   geom_point(aes(colour=factor(n_obs)), size = 3) +
#   facet_grid(pc~m2,labeller = label_both) + 
#   geom_vline(aes(xintercept=0.95),linetype=2) + 
#   theme_bw(base_size = 15) +
#   labs( x = "Coverage rate",
#         y = "Method",
#         color = "Drop-out rate",
#         shape = "Sample size") +
#   theme(legend.position = 'bottom')

saveRDS(p2_full, "summaries/p2_full.rds")

###################
####    MCAR   ####
###################

setting <- readRDS('setting.rds')
set <- seq(1,16,1)

#check target drop out rate
do_rate_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mcar/p2_mcar_set_n%s.rds", set_n))
  do_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(do_diff = mean_do - do_rate)
  
#check target group difference
m2_est_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mcar/p2_mcar_set_n%s.rds", set_n))
  m2_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(m2_diff = mean_m2 - m2)




p2_mcar_eval <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_mcar/p2_mcar_set_n%s.rds", set_n))

  x1 <- bin2mi::p2_eval(x, m2 = setting$m2[setting$set_n==set_n])
  x2 <- x1%>%
    dplyr::mutate(set_n = set_n, 
                  n_exc = purrr::keep(x, .p=function(x) is.character(x[[1]]))%>%
                    length()%>%
                    as.numeric())
})%>%
  left_join(setting, by = 'set_n')


saveRDS(p2_mcar_eval, "summaries/p2_mcar_eval.rds")

#######################
### MAR - strong   ###
#######################

setting <- readRDS('setting.rds')
set <- seq(1,16,1)

#check target drop out rate
do_rate_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mars/p2_mar_strong_set_n%s.rds", set_n))
  do_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(do_diff = round(mean_do - do_rate,4))

#check target group difference
m2_est_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mars/p2_mar_strong_set_n%s.rds", set_n))
  m2_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(m2_diff = mean_m2 - m2)




p2_mars_eval <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_mars/p2_mar_strong_set_n%s.rds", set_n))
  
  x1 <- bin2mi::p2_eval(x, m2 = setting$m2[setting$set_n==set_n])
  x2 <- x1%>%
    dplyr::mutate(set_n = set_n, 
                  n_exc = purrr::keep(x, .p=function(x) is.character(x[[1]]))%>%
                    length()%>%
                    as.numeric())
})%>%
  left_join(setting, by = 'set_n')

saveRDS(p2_mars_eval, "summaries/p2_mars_eval.rds")


#######################
### MAR - weak     ###
#######################

setting <- readRDS('setting.rds')
set <- seq(1,16,1)

#check target drop out rate
do_rate_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_marw/p2_mar_weak_set_n%s.rds", set_n))
  do_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(do_diff = round(mean_do - do_rate, 4))

#check target group difference
m2_est_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_marw/p2_mar_weak_set_n%s.rds", set_n))
  m2_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(m2_diff = mean_m2 - m2)




p2_marw_eval <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_marw/p2_mar_weak_set_n%s.rds", set_n))
  
  x1 <- bin2mi::p2_eval(x, m2 = setting$m2[setting$set_n==set_n])
  x2 <- x1%>%
    dplyr::mutate(set_n = set_n, 
                  n_exc = purrr::keep(x, .p=function(x) is.character(x[[1]]))%>%
                    length()%>%
                    as.numeric())
})%>%
  left_join(setting, by = 'set_n')

saveRDS(p2_marw_eval, "summaries/p2_marw_eval.rds")


#######################
### MNAR           ###
#######################

setting <- readRDS('setting.rds')
set <- seq(1,16,1)

#check target drop out rate
do_rate_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mnar_new/p2_mnar_set_new_n%s.rds", set_n))
  do_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(do_diff = mean_do - do_rate)

#check target group difference
m2_est_ch <- map_df(set, .f = function(set_n){
  x <- readRDS(sprintf("results/p2_mnar_new/p2_mnar_set_new_n%s.rds", set_n))
  m2_ch(x)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')%>%
  mutate(m2_diff = mean_m2 - m2)


p2_mnar_eval <- map_df(set, .f = function(set_n){
  
  x <- readRDS(sprintf("results/p2_mnar_new/p2_mnar_set_new_n%s.rds", set_n))
  
  x1 <- bin2mi::p2_eval(mi_level = 2, x, m2 = setting$m2[setting$set_n==set_n])
  x2 <- x1%>%
    dplyr::mutate(set_n = set_n, 
                  n_exc = purrr::keep(x, .p=function(x) is.character(x[[1]]))%>%
                    length()%>%
                    as.numeric())
})%>%
  left_join(setting, by = 'set_n')

saveRDS(p2_mnar_eval, "summaries/p2_mnar_eval.rds")


