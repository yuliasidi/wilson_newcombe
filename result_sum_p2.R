library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)

source("funs/m2_ch.R")
source("funs/do_ch.R")


cca_ch(p2_mcar_set_n1)
bin2mi::p2_eval(p2_mcar_set_n1, m2 = 0.025)


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

p2_mcar_eval%>%ggplot(aes(x=mean_cov,y=method)) +
  geom_point(aes(colour=factor(do_rate),shape=factor(n_obs))) +
  facet_grid(pc~m2,labeller = label_both) + 
  geom_vline(aes(xintercept=0.95),linetype=2) + 
  theme_bw()
