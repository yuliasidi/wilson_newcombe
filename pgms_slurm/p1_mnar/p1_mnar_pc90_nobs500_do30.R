library(dplyr, warn.conflicts = F, quietly = T)


source("wn/funs/mi.R")
source("wn/funs/mi.comb.R")
source("wn/funs/lb.wn.nonign.R")
source("wn/funs/ub.wn.nonign.R")
source("wn/funs/lb.wn.R")
source("wn/funs/ub.wn.R")
source("wn/funs/miss.cond.mnar.R")
source("wn/funs/sf.count.R")

pc <- 0.90
nobs <- 500
num.n.mi <- 2
num.m.mi <- 100


do.rate <-0.3
py1.cond <- 0.325

mu.k <- 1.4
sd.k <- 0.2

library(parallel)
cl <- makeCluster(Sys.getenv()["SLURM_NTASKS"], type = "MPI")

system.time({
  
  parallel::clusterExport(cl, varlist = ls())
  
  x1 <- 
    parallel::clusterApply(cl,
                           x = 1:10000, 
                           fun=function(x){
                             
 library(dplyr, warn.conflicts = F, quietly = T)
 library(purrr, warn.conflicts = F, quietly = T)
 library(tidyr, warn.conflicts = F, quietly = T)
                             
set.seed(round(1000*do.rate*pc,0) + x)

py0.cond <- 
  miss.cond.mnar(do.target = do.rate, p.cond = py1.cond, p.y1 = pc)

 dtfull <- 
   dplyr::tibble(
     y = rbinom (n = nobs, 1, prob = pc)) 
 
 #fully obnserved data analysis
 full.sum <- 
   dtfull%>%
   dplyr::summarise(phat = mean(y), nobs = n())

 
 full.sum.all <- 
 full.sum%>%
   dplyr::mutate(method = 'wald',
                 lower.bound = phat - qnorm(0.975)*sqrt(phat*(1 - phat)/nobs),
                 upper.bound = phat + qnorm(0.975)*sqrt(phat*(1 - phat)/nobs))%>%
   bind_rows(full.sum%>%
               dplyr::mutate(method = 'wn',
                             lower.bound = pmap_dbl(list(phat = phat,nobs = nobs), lb.wn, z = qnorm(0.975)),
                             upper.bound = pmap_dbl(list(phat = phat,nobs = nobs), ub.wn, z = qnorm(0.975)))
   )%>%
   dplyr::mutate(sim.id = x)

 
  #missing data generation
 dtmiss <- 
   dtfull%>%
   dplyr::filter(y == 1)%>%
   dplyr::mutate(r = rbinom(n(), 1, py1.cond))%>%
   dplyr::bind_rows(
     dtfull%>%
       dplyr::filter(y == 0)%>%
       dplyr::mutate(r = rbinom(n(), 1, py0.cond)))%>%
   dplyr::mutate(
     y.m = ifelse(r==1 ,NA_integer_ ,y)
   )
 
 #check do rate
 do.check <- dtmiss%>%
   dplyr::summarise(do = mean(r))
 
 #check phat for CCA
 phat.cca <- mean(dtmiss$y.m, na.rm = T)
 
 #MI
 dt.mi <- 
   dtmiss%>%
   mi(n.mi = num.n.mi, m.mi = num.m.mi, mu.k = mu.k, sd.k = sd.k)
 

 #define variance terms and combine MI results 
 dt.mi1<-
   dt.mi%>%
   dplyr::mutate(phat.var = phat*(1 - phat)/nobs)%>%
   mi.comb(level=2, phat = 'phat', var.phat = 'phat.var')

 #calculate CIs   
  wald <-
    dt.mi1%>%
    dplyr::mutate(method = "wald",
                 lower.bound = qbar - qnorm(0.975)*sqrt(t),
                 upper.bound = qbar + qnorm(0.975)*sqrt(t))
 
 wn.plug <- 
   dt.mi1%>%
   dplyr::mutate(method = "wn-plug",
                 lower.bound = pmap_dbl(list(phat = qbar), lb.wn, z = qnorm(0.975), nobs = nobs),
                 upper.bound = pmap_dbl(list(phat = qbar), ub.wn, z = qnorm(0.975), nobs = nobs))
 
 wn.mi <- 
   dt.mi1%>%
   dplyr:::mutate(method = "wn-mi", 
                  lower.bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn, sn = sn),
                                         lb.wn.nonign, nobs = nobs),
                  upper.bound  = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn, sn = sn),
                                          ub.wn.nonign, nobs = nobs))
 
 
 mi.sum.all <-
   bind_rows(wald, wn.plug, wn.mi)%>%
   dplyr::mutate(sim.id = x,
                 mu.k = mu.k,
                 sd.k = sd.k)
 
 
 ci.all <- list(full.sum.all, do.check, phat.cca, mi.sum.all)%>%
   purrr::set_names(c("full.sum.all", "do.check", "phat.cca", "mi.sum.all")) 
 
 return(ci.all)
})

})


saveRDS(x1, sprintf("results/p1_mnar_pc%d_nobs%d_do%d_mu%s_sd%s.rds",
                    round(100*pc,0), nobs, round(100*do.rate,0), mu.k, sd.k))
