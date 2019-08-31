library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)


source("funs/mi.R")
source("funs/mi.comb.R")
source("funs/lb.wn.nonign.R")
source("funs/ub.wn.nonign.R")
source("funs/lb.wn.R")
source("funs/ub.wn.R")
source("funs/miss.cond.mnar.R")
source("funs/sf.count.R")

pc <- 0.65
nobs <- 100
num.n.mi <- 2
num.m.mi <- 100


do.rate <-0.3
py1.cond <- 0.42

py0.cond <- 
  miss.cond.mnar(do.target = do.rate, p.cond = py1.cond, p.y1 = pc)

mu.k <- 1.5
sd.k <- 0.1
system.time({
x1 <- parallel::mclapply(X = 1:100, 
                         mc.cores = 7,
                         FUN= function(x) 
 
{
 
set.seed(round(1000*do.rate*pc,0) + x)
                           
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
   dplyr::mutate(sim.id = x)
 
 
 ci.all <- list(full.sum.all, do.check, phat.cca, mi.sum.all)%>%
   purrr::set_names(c("full.sum.all", "do.check", "phat.cca", "mi.sum.all")) 
 
 return(ci.all)
})

})


#check mean drop-out rate
x1%>%
  purrr::map_df(.f=function(x) x$do.check)%>%
  summarise(mean(do))

#mean phat for cca
x1%>%
  purrr::map_dbl(.f=function(x) x$phat.cca)%>%
  mean
  
  dplyr::mutate(lb.wald.cca = phat - qnorm(0.975)*sqrt(phat*(1 - phat)/nobs),
                ub.wald.cca = phat + qnorm(0.975)*sqrt(phat*(1 - phat)/nobs),
                wald.cp = ifelse(pc<=ub.wald.cca & pc>=lb.wald.cca,1,0))


x1%>%
  purrr::map_df(.f=function(x) x$mi.sum.all,.id = 'sim')%>%
  dplyr::mutate(coverage = ifelse(pc<=upper.bound & pc>=lower.bound,1,0),
                ci.length = upper.bound - lower.bound,
                out01 = ifelse(upper.bound>1 | lower.bound<0, 1, 0)
  )%>%
  dplyr::group_by(method)%>%
  dplyr::summarise(mean.phat = mean(qbar), mean.cov = mean(coverage), 
                   mean.length = mean(ci.length), mean.out01 = mean(out01))

saveRDS(x1, sprintf("results/p1_mnar_pc%d_nobs%d_n%d_m%d_do%d_mu%s_sd%s.rds",
                    round(100*pc,0), nobs, num.n.mi, num.m.mi, round(100*do.rate,0), mu.k, sd.k))
