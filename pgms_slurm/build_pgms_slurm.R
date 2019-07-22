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
                 file = file.path('pgms_slurm/p2_mars',
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
                 file = file.path('pgms_slurm/p2_mars',
                                  sprintf("p2_mar_weak_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

####################################
## p2 - SLURM, MNAR               ##
####################################

setting_ymnar <- readRDS("setting_ymnar.rds")

setting_ymnar.l <- as.list(setting_ymnar)

purrr::pwalk(.l = setting_ymnar.l,
             .f = function(pc, m2, do_rate, n_obs, set_n, mp_y1){
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
                               mu_k = 1.8,
                               sd_k = 0.3)
                 ),
                 file = file.path('pgms_slurm/p2_mnar',
                                  sprintf("p2_mnar_set_n%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

