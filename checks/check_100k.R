library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)

source("funs/do.cca.check.R")
source("funs/p1.eval.R")
source("funs/p1.eval.full.R")




t <- readRDS("results/p1_mnar/p1_mnar_pc65_nobs100_do30_mu1.8_sd0.3_100k.rds")
sum.prop1 <- readRDS("summaries/sum.prop1.rds")

p1.eval(t, pc = 0.65)

sum.prop1%>%
  dplyr::filter(mu.k==1.8, sd.k==0.3, n==100, pc==0.65, do==0.3)



###############################################
source("funs/m2_ch.R")
source("funs/do_ch.R")

d10k <- readRDS("results/p2_mars/p2_mar_strong_set_n6.rds")
d100k <- readRDS("results/p2_mars/p2_mar_strong_set_n6_100k.rds")
do_ch(d10k)
do_ch(d100k)


m2_ch(d10k)
m2_ch(d100k)

bin2mi::p2_eval(d10k, m2 = setting$m2[setting$set_n==6])
bin2mi::p2_eval(d100k, m2 = setting$m2[setting$set_n==6])

purrr::keep(d10k, .p=function(d10k) is.character(d10k[[1]]))%>%length()
purrr::keep(d100k, .p=function(d100k) is.character(d100k[[1]]))%>%length()

