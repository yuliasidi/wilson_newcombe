library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)


source("funs/mi.mcar.R")
source("funs/lb.wn.ign.R")
source("funs/ub.wn.ign.R")
source("funs/lb.wn.ign.plug.R")
source("funs/ub.wn.ign.plug.R")



pc<-0.2
n<-100
do.rate <-0.1
num.n.mi <- 10


x1.1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 6,
                         FUN= function(x) 
                           
                         {
                           
set.seed(1987 + x)
                           
dtfull <- 
  dplyr::tibble(
    y = rbinom (n = n, 1, prob = pc),
    x.strong = case_when(y == 0 ~ rbinom(n = n, 1, prob = 0.6),
                         y == 1 ~ rbinom(n = n, 1, prob = 0.2)),
    x.weak = rbinom(n = n, 1, prob = 0.6) 
  )

dt.mnar <- dtfull%>%
  dplyr::filter(y == 1)%>%
  dplyr::mutate(r = rbinom(n(), 1, do.rate*n/n()))%>%
  dplyr::bind_rows(dtfull%>%
                     dplyr::filter(y == 0)%>%
                     dplyr::mutate(r = 0))%>%
  dplyr::mutate(
    y.m = ifelse(r==1 ,NA_integer_ ,y)
  )

do.check <- mean(dt.mnar$r)

n.mi <- seq(1,num.n.mi,1)

mi.sum <- map_df(n.mi, .f = mi.mcar, dt = dt.mnar, .id = 'n.mi')%>%
  dplyr::mutate(var.phat = phat*(1 - phat)/n)%>%
  dplyr::summarise(qbar = mean(phat),
                   ubar = mean(var.phat),
                   b = var(phat),
                   m = n(), 
                   nobs = mean(nobs))%>%
  dplyr::mutate(t = ubar + (m+1)/m*b,
                v = floor((m - 1)*(1 + ubar/((1+1/m)*b))^2),
                rm = (1 + 1/m)*b/ubar)%>%
  dplyr::mutate(lb.wn = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, n = nobs , rm = rm), lb.wn.ign),
                ub.wn = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, n = nobs , rm = rm), ub.wn.ign),
                lb.wn.plug = pmap_dbl(list(qhat = qbar, n = nobs), lb.wn.ign.plug, z = qnorm(0.975)),
                ub.wn.plug = pmap_dbl(list(qhat = qbar, n = nobs), ub.wn.ign.plug, z = qnorm(0.975)),
                lb.wald = qbar - qt(0.975, df = v)*sqrt(t),
                ub.wald = qbar + qt(0.975, df = v)*sqrt(t))%>%
  dplyr::mutate(sim.id = x)

ci.all <- list(mi.sum, do.check)%>%purrr::set_names(c("mi.sum", "do.check")) 

return(ci.all)
})

x2.1<-x1.1%>%
  purrr::map_df(.f=function(x) x$mi.sum,.id = 'sim')%>%
  dplyr::mutate(wn.p.inc = ifelse(pc<=ub.wn & pc>=lb.wn,1,0),
                wn.plug.p.inc = ifelse(pc<=ub.wn.plug & pc>=lb.wn.plug,1,0),
                wald.p.inc = ifelse(pc<=ub.wald & pc>=lb.wald,1,0),
                wn.ci.l = ub.wn - lb.wn,
                wald.ci.l = ub.wald - lb.wald)%>%
  dplyr::summarise(wn.cov.rate = mean(wn.p.inc),
                   wn.plug.cov.rate = mean(wn.plug.p.inc),
                   wald.cov.rate = mean(wald.p.inc),
                   wn.l = mean(wn.ci.l),
                   wald.l = mean(wald.ci.l))
