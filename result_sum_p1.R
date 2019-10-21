library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(bin2mi)

setting <- readRDS('setting.rds')
set <- seq(1,16,1)

p1_mcar_eval <- 
  map_df(set, .f = function(set_n){
  
  x0 <- readRDS(sprintf("results/p2_mcar/p2_mcar_set_n%s.rds", set_n))
  x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
  
  pc <- setting$pc[setting$set_n==set_n]
  pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
  
  xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
  xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
  
  x2 <- bind_rows(xc, xt)%>%
    dplyr::mutate(set_n = set_n)
})%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_mcar_eval, "summaries/p1_mcar_eval.rds")
 
# MAR strong
p1_mars_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_mars/p2_mar_strong_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_mars_eval, "summaries/p1_mars_eval.rds")


p1_marw_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_marw/p2_mar_weak_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_marw_eval, "summaries/p1_marw_eval.rds")

p1_mnar_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_mnar/p2_mnar_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_mnar_eval, "summaries/p1_mnar_eval.rds")


p1_mnar_ign_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_mnar_ign/p2_mnar_ign_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_mnar_ign_eval, "summaries/p1_mnar_ign_eval.rds")

p1_mnar_new_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_mnar_new/p2_mnar_new_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')

saveRDS(p1_mnar_new_eval, "summaries/p1_mnar_new_eval.rds")

p1_mnar_new_ign_eval <- 
  map_df(set, .f = function(set_n){
    
    x0 <- readRDS(sprintf("results/p2_mnar_new_ign/p2_mnar_new_ign_set_n%s.rds", set_n))
    x <- purrr::discard(x0, .p=function(x0) is.character(x0[[1]]))%>%head(10000)
    
    pc <- setting$pc[setting$set_n==set_n]
    pt <- setting$pc[setting$set_n==set_n] - setting$m2[setting$set_n==set_n]
    
    xc <- bin2mi::p1_eval(x, trt = 'c', ptrt = pc)
    xt <- bin2mi::p1_eval(x, trt = 't', ptrt = pt)
    
    x2 <- bind_rows(xc, xt)%>%
      dplyr::mutate(set_n = set_n)
  })%>%
  left_join(setting, by = 'set_n')


saveRDS(p1_mnar_new_ign_eval, "summaries/p1_mnar_new_ign_eval.rds")


