#check bias
library(dplyr)


p2_mcar_eval <- readRDS("summaries/p2_mcar_eval_new.rds")
p2_mars_eval <- readRDS("summaries/p2_mars_eval_new.rds")
p2_marw_eval <- readRDS("summaries/p2_marw_eval_new.rds")
p2_mnar_eval <- readRDS("summaries/p2_mnar_eval_final.rds")

mcar_b <- p2_mcar_eval%>%mutate(bias = (mean_qbar - m2)/m2)
mars_b <- p2_mars_eval%>%mutate(bias = (mean_qbar - m2)/m2)
marw_b <- p2_marw_eval%>%mutate(bias = (mean_qbar - m2)/m2)
mnar_b <- p2_mnar_eval%>%mutate(bias = (mean_qbar - m2)/m2)
