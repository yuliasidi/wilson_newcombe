#define scenarios for interest
library(dplyr)

setting <- expand.grid(pc = c(0.65, 0.9),
                                  m2 = c(0.025, 0.1),
                                  do_rate = c(0.1, 0.3),
                                  n_obs = c(100,500))

setting <- setting%>%
  dplyr::mutate(set_n = seq(1, length(setting$pc), 1))

saveRDS(setting, 'setting.rds')

#update setting with scenarios were n_obs = 30
setnew <- expand.grid(pc = c(0.65, 0.9),
                       m2 = c(0.025, 0.1),
                       do_rate = c(0.1, 0.3),
                       n_obs = c(30))

setnew <- setnew%>%
  dplyr::mutate(set_n = seq(17, 17 + length(setnew$pc) - 1, 1))

saveRDS(setnew, 'setnew.rds')

#update setting with scenarios were m2 = 0
setnew1 <- expand.grid(pc = c(0.65, 0.9),
                      m2 = c(0),
                      do_rate = c(0.1, 0.3),
                      n_obs =  c(100,500))

setnew1 <- setnew1%>%
  dplyr::mutate(set_n = seq(25, 25 + length(setnew1$pc) - 1, 1))

saveRDS(setnew1, 'setnew1.rds')


#update setting with scenarios were m2 = 0
setnew2 <- expand.grid(pc = c(0.99),
                       m2 = c(0, 0.025, 0.1),
                       do_rate = c(0.1, 0.3),
                       n_obs =  c(100,500))

setnew2 <- setnew2%>%
  dplyr::mutate(set_n = seq(33, 33 + length(setnew2$pc) - 1, 1))

saveRDS(setnew2, 'setnew2.rds')
