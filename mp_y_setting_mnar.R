library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)

setting <- readRDS('setting.rds')

set1 <- setting%>%
  dplyr::filter(n_obs==50)%>%
  dplyr::rename(p_y1 = pc,
                do_tar = do_rate)%>%
  dplyr::select(p_y1, do_tar)%>%
  dplyr::mutate(mp_y1 = case_when((p_y1 == 0.65 & do_tar == 0.1) ~ 0.135,
                                  (p_y1 == 0.65 & do_tar == 0.3) ~ 0.42,
                                  (p_y1 == 0.9  & do_tar == 0.1) ~ 0.107,
                                  (p_y1 == 0.9  & do_tar == 0.3) ~ 0.325))

set1%>%
  mutate(mp_y0_val = pmap_dbl(as.list(set1), mp_y0))

#check set1 for pt
set12 <- setting%>%
  dplyr::filter(n_obs==50)%>%
  dplyr::mutate(p_y1 = pc - m2)%>%
  dplyr::rename(do_tar = do_rate)%>%
  dplyr::select(p_y1, pc, do_tar)%>%
  dplyr::mutate(mp_y1 = case_when((pc == 0.65 & do_tar == 0.1) ~ 0.135,
                                  (pc == 0.65 & do_tar == 0.3) ~ 0.42,
                                  (pc == 0.9  & do_tar == 0.1) ~ 0.107,
                                  (pc == 0.9  & do_tar == 0.3) ~ 0.325))

set12%>%
  mutate(mp_y0_val = pmap_dbl(as.list(set12%>%
                                        dplyr::select(-pc)), mp_y0))

setting_ymnar <- setting%>%
  dplyr::mutate(mp_y1 = case_when((pc == 0.65 & do_rate == 0.1) ~ 0.135,
                                  (pc == 0.65 & do_rate == 0.3) ~ 0.42,
                                  (pc == 0.9  & do_rate == 0.1) ~ 0.107,
                                  (pc == 0.9  & do_rate == 0.3) ~ 0.325))

saveRDS(setting_ymnar, "setting_ymnar.rds")
