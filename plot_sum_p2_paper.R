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

############################################################
### Coverage rate/width for fully observed data - tables ###
############################################################
print(
  xtable::xtable(p2_full%>%filter(method%in%c('wn', 'wald'))%>%
                   select(method, mean_cov, set_n)%>%
                   tidyr::spread(key = "method", value = "mean_cov")%>%
                   left_join(setting, by = 'set_n')%>%
                   select(pc, m2, n_obs, wald, wn), digits = c(0,2,2,0,4,4)), 
  include.rownames=FALSE)


print(
  xtable::xtable(p2_full%>%filter(method%in%c('wn', 'wald'))%>%
                   select(method, mean_length, set_n)%>%
                   tidyr::spread(key = "method", value = "mean_length")%>%
                   left_join(setting, by = 'set_n')%>%
                   select(pc, m2, n_obs, wald, wn), digits = c(0,2,2,0,3,3)), 
  include.rownames=FALSE)


#plot_cvf(p2_full%>%filter(method%in%c('wn', 'wald')))

#############################################
### Coverage rate/width for MCAR - plots ###
#############################################

p2_mcar_cv <-
  plot_cv_paper(p2_mcar_eval, c(0.89,.97))

pdf("plots/forpaper/p2_mcar_cv.pdf")
p2_mcar_cv
dev.off()

p2_mcar_wid <-
  plot_wid_paper(p2_mcar_eval)

pdf("plots/forpaper/p2_mcar_wid.pdf")
p2_mcar_wid
dev.off()


#############################################
### Coverage rate/width for MARS -  plots ###
#############################################

p2_mars_cv <-
  plot_cv_paper(p2_mars_eval, lim = c(0.85,.981))

pdf("plots/forpaper/p2_mars_cv.pdf")
p2_mars_cv
dev.off()

p2_mars_wid <-
  plot_wid_paper(p2_mars_eval)

pdf("plots/forpaper/p2_mars_wid.pdf")
p2_mars_wid
dev.off()

#############################################
### Coverage rate/width for MARW -  plots ###
#############################################

p2_marw_cv <-
  plot_cv_paper(p2_marw_eval, lim = c(0.88,.975))

pdf("plots/forpaper/p2_marw_cv.pdf")
p2_marw_cv
dev.off()

p2_marw_wid <-
  plot_wid_paper(p2_marw_eval)

pdf("plots/forpaper/p2_marw_wid.pdf")
p2_marw_wid
dev.off()

#############################################
### Coverage rate/width for MNAR -  plots ###
#############################################

p2_mnar_cv <-
  plot_cv_paper(p2_mnar_eval, lim = c(0.84, .96), bks = c(0.85, 0.9, 0.93, 0.95))

pdf("plots/forpaper/p2_mnar_cv.pdf")
p2_mnar_cv
dev.off()

p2_mnar_wid <-
  plot_wid_paper(p2_mnar_eval)

pdf("plots/forpaper/p2_mnar_wid.pdf")
p2_mnar_wid
dev.off()