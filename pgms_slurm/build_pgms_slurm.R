library(dplyr)
library(purrr)


####################################
## p2 - SLURM, MCAR               ##
####################################

setting <- readRDS("setting.rds")

setting.l <- as.list(setting)

purrr::pwalk(.l = setting.l,
            .f = function(pc, m2, do_rate, n_obs, set_n){
              cat(
                whisker::whisker.render(
                  readLines('tmpls/p2_mcar.tmpl'),
                  data = list(pc = pc,
                              m2 = m2,
                              n_obs = n_obs,
                              do_rate = do_rate,
                              set_n = set_n,
                              num_n_mi = 10)
                ),
                file = file.path('pgms_slurm/p2_mcar',
                                 sprintf("p2_mcar_set_n%s.R",
                                         set_n)
                ),
                sep='\n')
            })


####################################
## p2 - SLURM, MAR, strong        ##
####################################

setting_xmar <- readRDS("setting_xmar.rds")

setting_xmars.l <- as.list(setting_xmar%>%
                             dplyr::filter(x_desc == 'strong')%>%
                             dplyr::select(- x_desc))

purrr::pwalk(.l = setting_xmars.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_x1_val){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mar_strong.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10,
                               mp_x1_val = mp_x1_val)
                 ),
                 file = file.path('pgms_slurm/p2_mar',
                                  sprintf("p2_mar_strong_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


####################################
## p2 - SLURM, MAR, weak        ##
####################################

setting_xmar <- readRDS("setting_xmar.rds")

setting_xmars.l <- as.list(setting_xmar%>%
                             dplyr::filter(x_desc == 'weak')%>%
                             dplyr::select(- x_desc))

purrr::pwalk(.l = setting_xmars.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_x1_val){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mar_weak.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10,
                               mp_x1_val = mp_x1_val)
                 ),
                 file = file.path('pgms_slurm/p2_mar',
                                  sprintf("p2_mar_weak_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR new           ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")
mnar_pms <- readRDS( "mnar_pms.rds")

setting_ymnar <- setting_ymnar%>%
  left_join(mnar_pms%>%select(-mu_k), by = "set_n")


setting_ymnar.l <- as.list(setting_ymnar)

purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1, sd_k){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar_new.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 2,
                               num_m_mi = 100,
                               mp_y1  = mp_y1,
                               sd_k = sd_k)
                 ),
                 file = file.path('pgms_slurm/p2_mnar',
                                  sprintf("p2_mnar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


####################################
## p2 - SLURM, MNAR new params    ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")

setting_ymnar.l <- as.list(setting_ymnar)

purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar_params.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 2,
                               num_m_mi = 100,
                               mp_y1  = mp_y1)
                 ),
                 file = file.path('pgms_slurm/p2_mnar/params',
                                  sprintf("p2_mnar_set_n%s_params.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR               ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")
mnar_pms <- readRDS( "mnar_pms.rds")

setting_ymnar <- setting_ymnar%>%
  left_join(mnar_pms, by = "set_n")%>%
  dplyr::mutate(mu_k = round((1 - do_rate)/(do_rate*(1 - mp_y1)) - (1 - do_rate)/do_rate, 3),
                mu_check = mu_k - mu_ksim)

setting_ymnar.l <- as.list(setting_ymnar%>%
                             dplyr::select(-c(mu_ksim, mu_check)))


purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1, mu_k, sd_k){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 2,
                               num_m_mi = 100,
                               mp_y1  = mp_y1,
                               mu_k = mu_k, sd_k = sd_k)
                 ),
                 file = file.path('pgms_slurm/p2_mnar',
                                  sprintf("p2_mnar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR ign          ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")

setting_ymnar.l <- as.list(setting_ymnar)

purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar_ign.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10,
                               mp_y1  = mp_y1)
                 ),
                 file = file.path('pgms_slurm/p2_mnar/ign',
                                  sprintf("p2_mnar_ign_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR new set-up    ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")
mnar_pms <- readRDS( "mnar_pms.rds")

setting_ymnar <- setting_ymnar%>%
  left_join(mnar_pms, by = "set_n")%>%
  dplyr::mutate(mu_k = round((1 - do_rate)/(do_rate*(1 - mp_y1)) - (1 - do_rate)/do_rate, 3),
                mu_check = mu_k - mu_ksim)

setting_ymnar.l <- as.list(setting_ymnar%>%
                             dplyr::select(-c(mu_ksim, mu_check)))


purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1, mu_k, sd_k){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar_new.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 2,
                               num_m_mi = 100,
                               mp_y1  = mp_y1,
                               mu_k = mu_k, sd_k = sd_k)
                 ),
                 file = file.path('pgms_slurm/p2_mnar_new',
                                  sprintf("p2_mnar_new_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR new setup ign ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")

setting_ymnar.l <- as.list(setting_ymnar)

purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mnar_new_ign.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10,
                               mp_y1  = mp_y1)
                 ),
                 file = file.path('pgms_slurm/p2_mnar_new_ign',
                                  sprintf("p2_mnar_new_ign_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MCAR for setnew    ##
####################################

setnew <- readRDS("setnew.rds")

setnew.l <- as.list(setnew)

purrr::pwalk(.l = setnew.l,
             .f = function(pc, m2, do_rate, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mcar.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10)
                 ),
                 file = file.path('pgms_slurm/p2_mcar',
                                  sprintf("p2_mcar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


####################################
## p2 - SLURM, MCAR for setnew1    ##
####################################

setnew1 <- readRDS("setnew1.rds")

setnew1.l <- as.list(setnew1)

purrr::pwalk(.l = setnew1.l,
             .f = function(pc, m2, do_rate, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mcar.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10)
                 ),
                 file = file.path('pgms_slurm/p2_mcar',
                                  sprintf("p2_mcar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


####################################
## p2 - SLURM, MCAR for setnew2    ##
####################################

setnew2 <- readRDS("setnew2.rds")

setnew2.l <- as.list(setnew2)

purrr::pwalk(.l = setnew2.l,
             .f = function(pc, m2, do_rate, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/p2_mcar.tmpl'),
                   data = list(pc = pc,
                               m2 = m2,
                               n_obs = n_obs,
                               do_rate = do_rate,
                               set_n = set_n,
                               num_n_mi = 10)
                 ),
                 file = file.path('pgms_slurm/p2_mcar',
                                  sprintf("p2_mcar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

