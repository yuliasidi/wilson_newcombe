library(dplyr)
library(ggplot2)

source("funs/plot_cv.R")
source("funs/plot_len.R")

p2_full <- readRDS("summaries/p2_full.rds")
p2_mcar_eval <- readRDS("summaries/p2_mcar_eval.rds")
p2_mars_eval <- readRDS("summaries/p2_mars_eval.rds")
p2_marw_eval <- readRDS("summaries/p2_marw_eval.rds")
p2_mnar_eval <- readRDS("summaries/p2_mnar_eval.rds")

##################################
### Coverage rate plots ##########
##################################

plot_cv(p2_full)

p2_mcar_cv <-
  plot_cv(p2_mcar_eval)

pdf("plots/p2_mcar_cv.pdf")
p2_mcar_cv
dev.off()

p2_mars_cv <-
  plot_cv(p2_mars_eval)

pdf("plots/p2_mars_cv.pdf")
p2_mars_cv
dev.off()

p2_marw_cv <-
  plot_cv(p2_marw_eval)

pdf("plots/p2_marw_cv.pdf")
p2_marw_cv
dev.off()


p2_mnar_cv_orig <- 
  plot_cv(p2_mnar_eval)

pdf("plots/p2_mnar_cv_orig.pdf")
p2_mnar_cv_orig
dev.off()



##################################
### Average length plots #########
##################################

p2_mcar_l <-
  plot_len(p2_mcar_eval)

pdf("plots/p2_mcar_l.pdf")
p2_mcar_l
dev.off()

p2_mars_l <-
  plot_len(p2_mars_eval)

pdf("plots/p2_mars_l.pdf")
p2_mars_l
dev.off()

p2_marw_l <-
  plot_len(p2_marw_eval)

pdf("plots/p2_marw_l.pdf")
p2_marw_l
dev.off()


p2_mnar_l_orig <- 
  plot_len(p2_mnar_eval)

pdf("plots/p2_mnar_l_orig.pdf")
p2_mnar_l_orig
dev.off()

p2_mnar_l_new <- 
  plot_len(p2_mnar_new)

pdf("plots/p2_mnar_l_new.pdf")
p2_mnar_l_new
dev.off()


