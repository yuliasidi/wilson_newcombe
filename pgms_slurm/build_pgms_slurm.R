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

