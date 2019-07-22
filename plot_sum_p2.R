library(dplyr)
library(ggplot2)

source("funs/plot_cv.R")
source("funs/plot_len.R")

p2_mcar_eval <- readRDS("summaries/p2_mcar_eval.rds")
p2_mars_eval <- readRDS("summaries/p2_mars_eval.rds")
p2_marw_eval <- readRDS("summaries/p2_marw_eval.rds")
p2_mnar_eval <- readRDS("summaries/p2_mnar_eval.rds")
p2_mnar_eval_diffk <- readRDS("summaries/p2_mnar_eval_diffk.rds")

p2_mnar_dk <- p2_mnar_eval_diffk%>%
  mutate(k_final = case_when(set_n == 12 ~ 1,
                             set_n == 7 ~ 1,
                             set_n == 13 ~ ifelse(mu_k == 2.3, 1, 0),
                             set_n == 15 ~ ifelse(mu_k == 2.1 & sd_k == 0.3, 1, 0),
                             set_n == 16 ~ ifelse(mu_k == 1.6, 1, 0),
                             set_n == 8 ~ ifelse(mu_k == 1.5, 1, 0)))%>%
  filter(k_final == 1)

p2_mnar_new <- p2_mnar_eval%>%
  filter(!set_n%in%c(7,8,12,13,15,16))%>%
  bind_rows(p2_mnar_dk)

##################################
### Coverage rate plots ##########
##################################

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

p2_mnar_cv_new <- 
  plot_cv(p2_mnar_new)

pdf("plots/p2_mnar_cv_new.pdf")
p2_mnar_cv_new
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


