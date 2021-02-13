library(dplyr)
library(ggplot2)

source("funs/plot_cv_paper.R")
source("funs/plot_wid_paper.R")

setting <- readRDS('setting.rds')
p2_full <- readRDS("summaries/p2_full.rds")
p2_mcar_eval <- readRDS("summaries/p2_mcar_eval_new.rds")
p2_mars_eval <- readRDS("summaries/p2_mars_eval_new.rds")
p2_marw_eval <- readRDS("summaries/p2_marw_eval_new.rds")
p2_mnar_eval <- readRDS("summaries/p2_mnar_eval_final.rds")
p2_mnar_ign_eval <- readRDS('summaries/p2_mnar_new_ign_eval.rds')

############################################################
### Coverage rate/width for fully observed data - tables ###
############################################################
print(
  xtable::xtable(p2_full%>%filter(method%in%c('wn', 'wald'))%>%
                   select(method, mean_cov, set_n)%>%
                   tidyr::spread(key = "method", value = "mean_cov")%>%
                   left_join(setting, by = 'set_n')%>%
                   select(pc, m2, n_obs, wald, wn) %>%
                   arrange(pc, m2, n_obs), digits = c(0,2,3,0,4,4)), 
  include.rownames=FALSE)


print(
  xtable::xtable(p2_full%>%filter(method%in%c('wn', 'wald'))%>%
                   select(method, mean_length, set_n)%>%
                   tidyr::spread(key = "method", value = "mean_length")%>%
                   left_join(setting, by = 'set_n')%>%
                   select(pc, m2, n_obs, wald, wn) %>%
                   arrange(pc, m2, n_obs), digits = c(0,2,3,0,3,3)), 
  include.rownames=FALSE)


#plot_cvf(p2_full%>%filter(method%in%c('wn', 'wald')))

#############################################
### Coverage rate/width for MCAR - plots ###
#############################################

p2_mcar_cv <-
  plot_cv_paper(p2_mcar_eval%>%filter(n_obs!=50))

pdf("plots/forpaper/p2_mcar_cv.pdf")
p2_mcar_cv
dev.off()

p2_mcar_cv_n50 <-
  plot_cv_paper(p2_mcar_eval)

pdf("plots/forpaper/p2_mcar_cv_n50.pdf")
p2_mcar_cv_n50 
dev.off()

p2_mcar_wid <-
  plot_wid_paper(p2_mcar_eval%>%filter(n_obs!=50))

pdf("plots/forpaper/p2_mcar_wid.pdf")
p2_mcar_wid
dev.off()


#############################################
### Coverage rate/width for MARS -  plots ###
#############################################

p2_mars_cv <-
  plot_cv_paper(p2_mars_eval%>%filter(n_obs!=50), lim = c(0.85,.981))

pdf("plots/forpaper/p2_mars_cv.pdf")
p2_mars_cv
dev.off()

p2_mars_wid <-
  plot_wid_paper(p2_mars_eval%>%filter(n_obs!=50))

pdf("plots/forpaper/p2_mars_wid.pdf")
p2_mars_wid
dev.off()

#############################################
### Coverage rate/width for MARW -  plots ###
#############################################

p2_marw_cv <-
  plot_cv_paper(p2_marw_eval%>%filter(n_obs!=50), lim = c(0.88,.975))

pdf("plots/forpaper/p2_marw_cv.pdf")
p2_marw_cv
dev.off()

p2_marw_wid <-
  plot_wid_paper(p2_marw_eval%>%filter(n_obs!=50))

pdf("plots/forpaper/p2_marw_wid.pdf")
p2_marw_wid
dev.off()

#############################################
### Coverage rate/width for MNAR -  plots ###
#############################################

p2_mnar_cv <-
  plot_cv_paper(p2_mnar_eval%>%filter(n_obs!=50), lim = c(0.84, .96), bks = c(0.85, 0.9, 0.93, 0.95))

pdf("plots/forpaper/p2_mnar_cv.pdf")
p2_mnar_cv
dev.off()

p2_mnar_wid <-
  plot_wid_paper(p2_mnar_eval%>%filter(n_obs!=50))

pdf("plots/forpaper/p2_mnar_wid.pdf")
p2_mnar_wid
dev.off()


#####################################################
## Coverage table for MNAR ign vs nonign for wn-mi ##
#####################################################
print(
  xtable::xtable(p2_mnar_eval%>%
                   filter(n_obs!=50)%>%
                   select(method, set_n, mean_cov)%>%
                   left_join(p2_mnar_ign_eval%>%
                               filter(n_obs!=50)%>%
                               select(method, set_n,mean_cov, pc, m2, do_rate
                                      ,n_obs)%>%
                               rename(mean_coving = mean_cov), by = c('method', 'set_n'))%>%
                   mutate(diff = mean_cov - mean_coving)%>%
                   filter(method == "wn-mi")%>%
                   arrange(pc, m2, n_obs)%>%
                   select(pc, m2, n_obs, do_rate, mean_cov, mean_coving, diff), digits = c(0,2,3,0,2,4,4,4)), 
  include.rownames=FALSE)

