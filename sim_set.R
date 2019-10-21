#define scenarios for interest
library(dplyr)

setting <- expand.grid(pc = c(0.65, 0.9),
                                  m2 = c(0.025, 0.1),
                                  do_rate = c(0.1, 0.3),
                                  n_obs = c(100,500))

setting <- setting%>%
  dplyr::mutate(set_n = seq(1, length(setting$pc), 1))

saveRDS(setting, 'setting.rds')

#update setting with scenarios were n_obs = 50
setnew <- expand.grid(pc = c(0.65, 0.9),
                       m2 = c(0.025, 0.1),
                       do_rate = c(0.1, 0.3),
                       n_obs = c(50))

setnew <- setnew%>%
  dplyr::mutate(set_n = seq(17, 17 + length(setnew$pc) - 1, 1))

setting <- setting%>%bind_rows(setnew)

saveRDS(setting, 'setting.rds')
