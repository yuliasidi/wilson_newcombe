library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi)

source('funs/neff_li.R')

p2_mcar_eval <- readRDS("summaries/p2_mcar_eval.rds")
p2_mars_eval <- readRDS("summaries/p2_mars_eval.rds")
p2_marw_eval <- readRDS("summaries/p2_marw_eval.rds")
#p2_mnar_eval <- readRDS("summaries/p2_mnar_eval.rds")
p2_mnar_new_eval <- readRDS("summaries/p2_mnar_new_eval.rds")

setting <- readRDS('setting.rds')
set <- seq(1,24,1)

neff_mcar <- map_df(set, neff_li, path_res = 'p2_mcar/p2_mcar_set')%>%
  dplyr::mutate(method = 'li')
p2_mcar_eval_new <- dplyr::bind_rows(p2_mcar_eval, neff_mcar)
saveRDS(p2_mcar_eval_new , "summaries/p2_mcar_eval_new.rds")

neff_mars <- map_df(set, neff_li, path_res = 'p2_mars/p2_mar_strong_set')%>%
  dplyr::mutate(method = 'li')

p2_mars_eval_new <- dplyr::bind_rows(p2_mars_eval, neff_mars)
saveRDS(p2_mars_eval_new , "summaries/p2_mars_eval_new.rds")

neff_marw <- map_df(set, neff_li, path_res = 'p2_marw/p2_mar_weak_set')%>%
  dplyr::mutate(method = 'li')

p2_marw_eval_new <- dplyr::bind_rows(p2_marw_eval, neff_marw)
saveRDS(p2_marw_eval_new , "summaries/p2_marw_eval_new.rds")

neff_mnar <- map_df(set, neff_li, path_res = 'p2_mnar/p2_mnar_set')%>%
  dplyr::mutate(method = 'li')

p2_mnar_eval_new <- dplyr::bind_rows(p2_mnar_eval, neff_mnar)
saveRDS(p2_mnar_eval_new , "summaries/p2_mnar_eval_new.rds")

# neff_mnar_kalt <- map_df(set, neff_li, path_res = 'p2_mnar/kalt/p2_mnar_set', kalt = '_kalt')%>%
#   dplyr::mutate(method = 'li')
# 
# p2_mnar_eval_kalt_new <- dplyr::bind_rows(p2_mnar_eval_kalt, neff_mnar_kalt)
# saveRDS(p2_mnar_eval_kalt_new , "summaries/p2_mnar_eval_kalt_new.rds")

neff_mnar_new <- map_df(set, neff_li, path_res = 'p2_mnar_new/p2_mnar_new_set')%>%
  dplyr::mutate(method = 'li')

p2_mnar_eval_final <- dplyr::bind_rows(p2_mnar_new_eval, neff_mnar_new)
saveRDS(p2_mnar_eval_final , "summaries/p2_mnar_eval_final.rds")
