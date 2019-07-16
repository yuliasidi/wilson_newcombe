library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)

source("wn/funs/dtfull.p2.create.R")
source("wn/funs/mi.R")
source("wn/funs/mi.comb.R")
source("wn/funs/p.rmle.fm.R")
source("wn/funs/lb.wn.R")
source("wn/funs/ub.wn.R")
source("wn/funs/lb.wn.ign.R")
source("wn/funs/ub.wn.ign.R")
source("wn/funs/lb.wn.p2.R")
source("wn/funs/ub.wn.p2.R")
source("wn/funs/sf.count.R")


pc <- 0.65
m2 <- 0.1
pt <- pc - m2
nobs <- 500
do.rate <- 0.1
num.n.mi <- 10

x1 <- parallel::mclapply(X = 1:10000, 
                         mc.cores = 10,
                         FUN= function(x) 
                           
                         {

library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
                           
set.seed(round(1000*do.rate*pc*m2,0) + x)


###################################################
#   fully obnserved data generation and analysis  #
###################################################
                           
dtfull <- dtfull.p2.create(n = nobs, pc = pc, pt = pt)


full.sum <- 
  dtfull%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise(phat = mean(y), nobs = n())%>%
  tidyr::gather('var','val',-trt)%>%
  tidyr::unite(x,trt,var)%>%
  tidyr::spread(x,val)%>%
  dplyr::mutate(phat.d = c_phat - t_phat,
                var.d = c_phat*(1 - c_phat)/c_nobs + t_phat*(1 - t_phat)/t_nobs,
                pc.rmle = p.rmle.fm(M2 = m2, n_T = t_nobs, n_C = c_nobs, p_C = c_phat, p_T = t_phat),
                pt.rmle = pc.rmle - m2,
                var.d.rmle = pc.rmle*(1 - pc.rmle)/c_nobs + pt.rmle*(1 - pt.rmle)/t_nobs)

full.sum.all <- 
  full.sum%>%
  dplyr::mutate( method = 'wald',
                 lower.bound = phat.d - qnorm(0.975)*sqrt(var.d),
                 upper.bound = phat.d + qnorm(0.975)*sqrt(var.d))%>%
  dplyr::bind_rows(full.sum%>%
                     dplyr::mutate( method = 'fm',
                                    lower.bound = phat.d - qnorm(0.975)*sqrt(var.d.rmle),
                                    upper.bound = phat.d + qnorm(0.975)*sqrt(var.d.rmle)))%>%
  dplyr::select(phat.d, c_nobs, method, lower.bound, upper.bound)%>%
  dplyr::bind_rows(full.sum%>%
                     dplyr::mutate(method = 'wn',
                                   lb.pc = pmap_dbl(list(phat = c_phat, nobs = c_nobs), lb.wn, z = qnorm(0.975)),
                                   ub.pc = pmap_dbl(list(phat = c_phat, nobs = c_nobs), ub.wn, z = qnorm(0.975)),
                                   lb.pt = pmap_dbl(list(phat = t_phat, nobs = t_nobs), lb.wn, z = qnorm(0.975)),
                                   ub.pt = pmap_dbl(list(phat = t_phat, nobs = t_nobs), ub.wn, z = qnorm(0.975)))%>%
                     dplyr::mutate(lower.bound = pmap_dbl(list(pc = c_phat, lb.pc = lb.pc, lb.pt = lb.pt,
                                                               ub.pc = ub.pc, ub.pt = ub.pt, pt = t_phat), 
                                                          lb.wn.p2),
                                   upper.bound = pmap_dbl(list(pc = c_phat, lb.pc = lb.pc, lb.pt = lb.pt,
                                                               ub.pc = ub.pc, ub.pt = ub.pt, pt = t_phat),
                                                          ub.wn.p2))%>%
                     dplyr::select(phat.d, c_nobs, method, lower.bound, upper.bound)
                     
  )%>%
  dplyr::mutate(sim.id = x,
                pc = pc,
                m2 = m2,
                do = do.rate)%>%
  dplyr::rename(nobs = c_nobs)

  
########################################
# missing data generation and analysis #
########################################

dtmiss <- 
  dtfull%>%
  split(.$trt)%>%
  purrr::map_df(.f = function(x){
    x%>%
      dplyr::mutate(r = rbinom(n(), 1, prob = do.rate),
                    y.m = ifelse(r==1 ,NA_integer_ ,y))
  })
  
#check do rate
do.check <- dtmiss%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise(do = mean(r))%>%
  dplyr::mutate(pc = pc,
                m2 = m2,
                do = do.rate,
                nobs = nobs)

#MI
dt.mi <- 
  dtmiss%>%
  split(.$trt)%>%
  purrr::map_df(mi, n.mi = num.n.mi, .id='trt')

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
  dplyr::mutate(sim.id = x,
                nobs = nobs,
                pc = pc,
                m2 = m2,
                do = do.rate)

ci.all <- list(full.sum.all, mi.sum.all, do.check)%>%purrr::set_names(c("full.sum.all", "mi.sum.all", "do.check")) 

return(ci.all)
})

saveRDS(x1, sprintf("results/p2_mcar_pc%s_m%s_nobs%s_do%s.rds",
                    round(100*pc,0), m2, nobs, round(100*do.rate,0)))
