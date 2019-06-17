library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)


source("funs/mi.mcar.R")
source("funs/mi.mnar.R")
source("funs/lb.wn.nonign.R")
source("funs/ub.wn.nonign.R")
source("funs/lb.wn.ign.plug.R")
source("funs/ub.wn.ign.plug.R")



pc<-0.2
n<-100
do.rate <-0.1
num.n.mi <- 2


x1 <- parallel::mclapply(X = 1:1000, 
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

mi.sum.mn <- map_df(n.mi, .f = mi.mnar, dt = dt.mnar, m.mi = 100, mu.k = 2, sd.k = 0.2, .id = 'n.mi')%>%
  dplyr::mutate(var.phat = phat*(1 - phat)/n)

sum.by.m <- 
  mi.sum.mn%>%
  dplyr::group_by(m)%>%
  dplyr::summarise(qbar.m = mean(phat),
                   q.m.var = var(phat),
                   ubar.m = mean(var.phat),
                   n = n())

#overall summary
mi.sum <- 
  sum.by.m%>%
  dplyr::summarise(qbar = mean(qbar.m),
                   ubar = mean(ubar.m),
                   b = var(qbar.m),
                   w = mean(q.m.var),
                   n = mean(n),
                   m = n())%>%
  dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                v = floor(1/v_1)
  )%>%
  dplyr::mutate(lb.wn = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, n = 100 , b = b, w = w,  N = n, M = m),
                                 lb.wn.nonign),
                ub.wn = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, n = 100 , b = b, w = w,  N = n, M = m),
                                 ub.wn.nonign))%>%
  dplyr::mutate(sim.id = x)



ci.all <- list(mi.sum, do.check)%>%purrr::set_names(c("mi.sum", "do.check")) 

return(ci.all)
})

x2<-x1%>%
  purrr::map_df(.f=function(x) x$mi.sum,.id = 'sim')%>%
  dplyr::mutate(wn.p.inc = ifelse(pc<=ub.wn & pc>=lb.wn,1,0))%>%
  dplyr::summarise(wn.cov.rate = mean(wn.p.inc, na.rm = T))

