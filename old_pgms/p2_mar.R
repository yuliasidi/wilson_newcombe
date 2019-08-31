library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(bin2mi, warn.conflicts = F, quietly = T)


pc <- 0.65
m2 <<- 0.1
pt <- pc - m2
n_obs <- 100
do_rate <- 0.3
num_n_mi <- 10
set_n <- 1


x1 <- parallel::mclapply(X = 1:10000, 
                         mc.cores = 6,
                         FUN= function(x) 
                           
                         {
                           
set.seed(20000*set_n + x)


###################################################
#   fully obnserved data generation and analysis  #
###################################################
                           
dtfull <- dt_p2(n = n_obs, pc = pc, pt = pt, add_x = T)


full_mle <- p2_mle(dtfull%>%
                     dplyr::filter(x_desc == "strong"), m2 = m2)

full_ci <- 
  bind_rows(p2_full_ci(full_mle, "wald", 0.05),
            p2_full_ci(full_mle, "fm", 0.05),
            p2_full_ci(full_mle, "wn", 0.05))%>%
  dplyr::mutate(sim_id = x,
                set_n = set_n)%>%
  dplyr::rename(n_obs = c_n_obs)

  
########################################
# missing data generation and analysis #
########################################

#calculate conditional probabilities forr missingness for x = 0

mp_dt <- expand.grid(trt = c('c', 't'),
                     x_desc = c('weak', 'strong'))

mp_dt <- mp_dt%>%
  dplyr::mutate(mp_x1 = ifelse(x_desc == 'strong', 0.6, 0.4))%>%
  dplyr::mutate(mp_x0 = ifelse(trt == 'c', 
                  pmap_dbl(list(mp_x1 = mp_x1, xs_ass = x_desc), mp_x0, do_tar = do_rate, p_y1 = pc),
                  pmap_dbl(list(mp_x1 = mp_x1, xs_ass = x_desc), mp_x0, do_tar = do_rate, p_y1 = pt)))

mp_dt <- mp_dt%>%
  tidyr::gather(key = 'x', 'mp_x', c(mp_x0, mp_x1) )%>%
  dplyr::mutate(x = ifelse(x == 'mp_x0', 0, 1))

suppressWarnings(dtmiss <- 
  dtfull%>%
    tidyr::nest(-c(trt, x, x_desc))%>%
    dplyr::left_join(mp_dt, by = c('trt', 'x_desc', 'x'))%>%
    dplyr::mutate(data_m = purrr::pmap(list(data, mp_x), 
                                   .f = function(dt, mp_x){
                                     dt%>%
                                       dplyr::mutate(r = rbinom(n(), 1, prob = mp_x),
                                                      y.m = ifelse(r==1 ,NA_integer_ ,y))
                                   }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest())

  
#check do rate
do_check <- dtmiss%>%
  dplyr::group_by(trt, x_desc)%>%
  dplyr::summarise(do = mean(r))

do_check_x <- dtmiss%>%
  dplyr::group_by(trt, x_desc, x)%>%
  dplyr::summarise(do = mean(r))


#MI
dt_mi <- 
  dtmiss%>%
  tidyr::nest(-c(trt, x, x_desc))%>%
  dplyr::mutate(data_im = purrr::map(data, mi, n_mi = num_n_mi, ym ='y.m', phat_out=F))%>%
  dplyr::select(- data)%>%
  tidyr::unnest()%>%
  tidyr::unnest()%>%
  dplyr::group_by(n, x_desc, trt)%>%
  dplyr::summarise(phat = mean(y.im), n_obs = dplyr::n())

#transpose the data, define variance terms to be used for combining MI results
dt.mi1 <-
  dt.mi%>%
  dplyr::mutate(phat.var = phat*(1 - phat)/nobs)%>%
  dplyr::filter(trt=='t')%>%
  dplyr::rename(phat.t = phat, nobs.t = nobs, phat.var.t = phat.var)%>%
  dplyr::select(-trt)%>%
  dplyr::left_join(dt.mi%>%
                     dplyr::mutate(phat.var = phat*(1 - phat)/nobs)%>%
                     dplyr::filter(trt=='c')%>%
                     dplyr::rename(phat.c = phat, nobs.c = nobs, phat.var.c = phat.var)%>%
                     dplyr::select(-trt), 
                   by = 'n')%>%
  dplyr::mutate(pc.rmle = p.rmle.fm(M2 = m2, n_T = nobs.t, n_C = nobs.t, p_C = phat.c, p_T = phat.t),
                pt.rmle = pc.rmle - m2)%>%
  dplyr::mutate(phat.d = phat.c - phat.t,
                var.d = phat.var.c + phat.var.t,
                var.d.rmle = pc.rmle*(1 - pc.rmle)/nobs.c + pt.rmle*(1 - pt.rmle)/nobs.t)


#combine MI results and calculate CIs using different methods

wald.res <-
  dt.mi1%>%
  mi.comb(level=1, phat = 'phat.d', var.phat = 'var.d')%>%
  dplyr::mutate(method = "wald",
                lower.bound = qbar - qnorm(0.975)*sqrt(t),
                upper.bound = qbar + qnorm(0.975)*sqrt(t))

fm.res <- 
  dt.mi1%>%
  mi.comb(level=1, phat = 'phat.d', var.phat = 'var.d.rmle')%>%
  dplyr::mutate(method = "fm",
                lower.bound = qbar - qnorm(0.975)*sqrt(t),
                upper.bound = qbar + qnorm(0.975)*sqrt(t))

wn.plug.pc <- 
  dt.mi1%>%
  mi.comb(level=1, phat = 'phat.c', var.phat = 'phat.var.c')%>%
  dplyr::mutate(method = "wn-plug for pc",
                lower.bound = pmap_dbl(list(phat = qbar), lb.wn, z = qnorm(0.975), nobs = nobs),
                upper.bound = pmap_dbl(list(phat = qbar), ub.wn, z = qnorm(0.975), nobs = nobs))

wn.plug.pt <- 
  dt.mi1%>%
  mi.comb(level=1, phat = 'phat.t', var.phat = 'phat.var.t')%>%
  dplyr::mutate(method = "wn-plug for pt",
                lower.bound = pmap_dbl(list(phat = qbar), lb.wn, z = qnorm(0.975), nobs = nobs),
                upper.bound = pmap_dbl(list(phat = qbar), ub.wn, z = qnorm(0.975), nobs = nobs))

wn.plug <- 
  tibble(method = "wn-plug", 
         pc = wn.plug.pc$qbar, lb.pc = wn.plug.pc$lower.bound, ub.pc = wn.plug.pc$upper.bound,
         pt = wn.plug.pt$qbar, lb.pt = wn.plug.pt$lower.bound, ub.pt = wn.plug.pt$upper.bound,
         qbar = wn.plug.pc$qbar - wn.plug.pt$qbar)%>%
  dplyr::mutate(lower.bound = pmap_dbl(list(pc, lb.pc, ub.pc, pt, lb.pt, ub.pt), lb.wn.p2),
                upper.bound = pmap_dbl(list(pc, lb.pc, ub.pc, pt, lb.pt, ub.pt), ub.wn.p2))%>%
  dplyr::select(method, qbar, lower.bound, upper.bound)

wn.mi.pc <- 
  dt.mi1%>%
  mi.comb(level=1, phat = 'phat.c', var.phat = 'phat.var.c')%>%
  dplyr::mutate(method = "wn-mi for pc",
                lower.bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb.wn.ign, nobs = nobs),
                upper.bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub.wn.ign, nobs = nobs))

wn.mi.pt <- 
dt.mi1%>%
  mi.comb(level=1, phat = 'phat.t', var.phat = 'phat.var.t')%>%
  dplyr::mutate(method = "wn-mi for pt",
                lower.bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb.wn.ign, nobs = nobs),
                upper.bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub.wn.ign, nobs = nobs))

wn.mi <- 
  tibble(method = "wn-mi", 
         pc = wn.mi.pc$qbar, lb.pc = wn.mi.pc$lower.bound, ub.pc = wn.mi.pc$upper.bound,
         pt = wn.mi.pt$qbar, lb.pt = wn.mi.pt$lower.bound, ub.pt = wn.mi.pt$upper.bound,
         qbar = wn.mi.pc$qbar - wn.mi.pt$qbar)%>%
  dplyr::mutate(lower.bound = pmap_dbl(list(pc, lb.pc, ub.pc, pt, lb.pt, ub.pt), lb.wn.p2),
                upper.bound = pmap_dbl(list(pc, lb.pc, ub.pc, pt, lb.pt, ub.pt), ub.wn.p2))%>%
  dplyr::select(method, qbar, lower.bound, upper.bound)
           
  
mi.sum.all <-
 bind_rows(wald.res, fm.res, wn.plug, wn.mi, wn.plug.pc, wn.plug.pt, wn.mi.pc, wn.mi.pt )%>%
  dplyr::mutate(sim.id = x)

ci.all <- list(full.sum.all, mi.sum.all, do.check)%>%purrr::set_names(c("full.sum.all", "mi.sum.all", "do.check")) 

return(ci.all)
})

