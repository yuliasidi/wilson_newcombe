library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)


source("wn/funs/mi.mnar.R")
source("wn/funs/lb.wn.nonign.R")
source("wn/funs/ub.wn.nonign.R")
source("wn/funs/lb.wn.R")
source("wn/funs/ub.wn.R")
source("wn/funs/miss.cond.mnar.R")



pc <- 0.65
nobs <- 100
num.n.mi <- 2
num.m.mi <- 100


do.rate <-0.1
py1.cond <- 0.12

py0.cond <- miss.cond.mnar(do.target = do.rate, p.cond = 0.12, p.y1 = pc)

x1 <- parallel::mclapply(X = 1:10, 
                         mc.cores = 6,
                         FUN= function(x) 
                           
                         {
                           
set.seed(1987 + x)
                           
dtfull <- 
  dplyr::tibble(
    y = rbinom (n = nobs, 1, prob = pc)) 
  


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

do.check <- mean(dtmiss$r)

phat.cca <- mean(dtmiss$y.m, na.rm = T)
  
mi.sum <- mi.mnar(dt = dtmiss, n.mi = 2, m.mi = num.m.mi, mu.k = 1.5, sd.k = 0.1)%>%
  dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                v = case_when(v_1!=0 ~ floor(1/v_1),
                              TRUE ~ as.numeric(1000000000))
  )%>%
  dplyr::mutate(lb.wn.mi = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, b = b, w = w,  n = n, m = m),
                                 lb.wn.nonign, nobs = nobs),
                ub.wn.mi = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, b = b, w = w,  n = n, m = m),
                                 ub.wn.nonign, nobs = nobs),
                lb.wn.plug = pmap_dbl(list(z = qnorm(0.975), phat = qbar), lb.wn, nobs = nobs),
                ub.wn.plug = pmap_dbl(list(z = qnorm(0.975), phat = qbar), ub.wn, nobs = nobs),
                lb.wald = qbar - qt(0.975, df = v)*sqrt(t),
                ub.wald = qbar + qt(0.975, df = v)*sqrt(t))%>%
  dplyr::mutate(sim.id = x)



ci.all <- list(mi.sum, do.check, phat.cca)%>%purrr::set_names(c("mi.sum", "do.check", "phat.cca")) 

return(ci.all)
})


#check mean drop-out rate
x1%>%
  purrr::map_dbl(.f=function(x) x$do.check)%>%
  mean

x1.cca <- tibble(phat = x1%>%
  purrr::map_dbl(.f=function(x) x$phat.cca))%>%
  dplyr::mutate(lb.wald.cca = phat - qnorm(0.975)*sqrt(phat*(1 - phat)/nobs),
                ub.wald.cca = phat + qnorm(0.975)*sqrt(phat*(1 - phat)/nobs),
                wald.cp = ifelse(pc<=ub.wald.cca & pc>=lb.wald.cca,1,0))

x1.cca%>%summarise_at(.vars = c("phat","wald.cp"), .funs = mean)
  
x1%>%
  purrr::map_df(.f=function(x) x$mi.sum,.id = 'sim')%>%
  dplyr::mutate(wn1 = ifelse(pc<=ub.wn.mi & pc>=lb.wn.mi,1,0),
                wn2 = ifelse(pc<=ub.wn.plug & pc>=lb.wn.plug,1,0),
                wald = ifelse(pc<=ub.wald & pc>=lb.wald,1,0)
                )%>%
  dplyr::summarise(mean(wn1), mean(wn2), mean(wald))

