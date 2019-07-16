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
