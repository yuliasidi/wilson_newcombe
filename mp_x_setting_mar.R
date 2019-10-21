library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

setting <- readRDS('setting.rds')

set1 <- setting%>%
  dplyr::rename(p_y1 = pc,
                do_tar = do_rate)%>%
  dplyr::select(p_y1, do_tar)%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar==0.1, 0.2, 0.6))

set1%>%
  mutate(mp_x0_val = pmap_dbl(as.list(set1), mp_x0, xs_ass = 'strong'))

#check set1 for pt
set12 <- setting%>%
  dplyr::mutate(p_y1 = pc - m2)%>%
  dplyr::rename(do_tar = do_rate)%>%
  dplyr::select(p_y1, do_tar)%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar==0.1, 0.2, 0.6))

set12%>%
  mutate(mp_x0_val = pmap_dbl(as.list(set12), mp_x0, xs_ass = 'strong'))

###################

set2 <- setting%>%
  dplyr::rename(p_y1 = pc,
                do_tar = do_rate)%>%
  dplyr::select(p_y1, do_tar)%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar==0.1, 0.15, 0.4))


set2%>%
  mutate(mp_x0_val = pmap_dbl(as.list(set2), mp_x0, xs_ass = 'weak'))

#check set2 for pt

set22 <- setting%>%
  dplyr::mutate(p_y1 = pc - m2)%>%
  dplyr::rename(do_tar = do_rate)%>%
  dplyr::select(p_y1, do_tar)%>%
  dplyr::mutate(mp_x1 = ifelse(do_tar==0.1, 0.15, 0.4))

set22%>%
  mutate(mp_x0_val = pmap_dbl(as.list(set22), mp_x0, xs_ass = 'weak'))

setting_xmar <- setting%>%
  dplyr::mutate(mp_x1_val = ifelse(do_rate==0.1, 0.2, 0.6),
                x_desc = 'strong')%>%
  bind_rows(setting%>%
              dplyr::mutate(mp_x1_val = ifelse(do_rate==0.1, 0.15, 0.4),
                            x_desc = 'weak'))

saveRDS(setting_xmar, "setting_xmar.rds")


