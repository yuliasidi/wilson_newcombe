library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)

source("funs/do.cca.check.R")
source("funs/p1.eval.R")
source("funs/p1.eval.full.R")

############################
### n=100, do=10%, pc =0.65
############################

map_df(.x = list.files("results", "mnar_pc65_nobs100_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc65.n100.do10 <- map_df(.x = list.files("results", "mnar_pc65_nobs100_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 100, do = 0.1)


sum.pc65.n100.full <-
  map_df(.x = list.files("results", "p1_mnar_pc65_nobs100_do10_mu1.8_sd0.1", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval.full(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 100, do = 0)


############################
### n=100, do=30%, pc =0.65
############################

map_df(.x = list.files("results", "mnar_pc65_nobs100_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc65.n100.do30 <- map_df(.x = list.files("results", "mnar_pc65_nobs100_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 100, do = 0.3)

############################
### n=500, do=10%, pc =0.65
############################

map_df(.x = list.files("results", "mnar_pc65_nobs500_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc65.n500.do10 <- map_df(.x = list.files("results", "mnar_pc65_nobs500_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 500, do = 0.1)



sum.pc65.n500.full <-
  map_df(.x = list.files("results", "p1_mnar_pc65_nobs500_do10_mu1.8_sd0.2", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval.full(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 500, do = 0)

############################
### n=500, do=30%, pc =0.65
############################

map_df(.x = list.files("results", "mnar_pc65_nobs500_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc65.n500.do30 <- map_df(.x = list.files("results", "mnar_pc65_nobs500_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.65)
})%>%
  dplyr::mutate(pc = 0.65, n = 500, do = 0.3)


############################
### n=100, do=10%, pc =0.90
############################

map_df(.x = list.files("results", "mnar_pc90_nobs100_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc90.n100.do10 <- map_df(.x = list.files("results", "mnar_pc90_nobs100_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 100, do = 0.1)


sum.pc90.n100.full <-
  map_df(.x = list.files("results", "p1_mnar_pc90_nobs100_do10_mu1.4_sd0.2", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval.full(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 100, do = 0)


############################
### n=100, do=30%, pc =0.90
############################

map_df(.x = list.files("results", "mnar_pc90_nobs100_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc90.n100.do30 <- map_df(.x = list.files("results", "mnar_pc90_nobs100_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 100, do = 0.3)

############################
### n=500, do=10%, pc =0.90
############################

map_df(.x = list.files("results", "mnar_pc90_nobs500_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc90.n500.do10 <- map_df(.x = list.files("results", "mnar_pc90_nobs500_do10", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 500, do = 0.1)


sum.pc90.n500.full <-
  map_df(.x = list.files("results", "p1_mnar_pc90_nobs100_do30_mu1.32_sd0.025", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval.full(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 500, do = 0)


############################
### n=500, do=30%, pc =0.90
############################

map_df(.x = list.files("results", "mnar_pc90_nobs500_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%do.cca.check()
})


sum.pc90.n500.do30 <- map_df(.x = list.files("results", "mnar_pc90_nobs500_do30", full.names = T), .f = function(x) {
  dt <- readRDS(x)
  dt%>%p1.eval(pc=0.9)
})%>%
  dplyr::mutate(pc = 0.90, n = 500, do = 0.3)





##########################################
### Overall summaries for 1 proportion ###
##########################################

sum.prop1 <-
  bind_rows(sum.pc65.n100.do10,
            sum.pc65.n100.do30,
            sum.pc65.n500.do10,
            sum.pc65.n500.do30,
            sum.pc65.n100.full,
            sum.pc65.n500.full,
            sum.pc90.n100.do10,
            sum.pc90.n100.do30,
            sum.pc90.n500.do10,
            sum.pc90.n500.do30,
            sum.pc90.n100.full,
            sum.pc90.n500.full)

saveRDS(sum.prop1, "summaries/sum.prop1.rds")
