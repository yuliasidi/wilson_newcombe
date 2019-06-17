library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)


spec <- expand.grid(pc = c(0.65), n = 100, do = 0.1)


pc<-0.65
n<-100
do <-0.1

sim.dt <- spec%>%
  dplyr::mutate(dt = map2(pc, n, .f = function(pc, n){
    
    tibble(
    y = rbinom (n = n, 1, prob = pc),
    x.strong = case_when(y == 0 ~ rbinom(n = n, 1, prob = 0.6),
                         y == 1 ~ rbinom(n = n, 1, prob = 0.2)),
    x.weak = rbinom(n = n, 1, prob = 0.6) 
    )
  }))


dt <- 
  tibble(
  y = rbinom (n = n, 1, prob = pc),
  x.strong = case_when(y == 0 ~ rbinom(n = n, 1, prob = 0.6),
                       y == 1 ~ rbinom(n = n, 1, prob = 0.2)),
  x.weak = rbinom(n = n, 1, prob = 0.6) 
)

dt.mcar <- dt%>%
  dplyr::mutate(
    r = rbinom(n(), 1, prob = do),
    y.m = ifelse(r==1 ,NA_integer_ ,y))


dt.mar.xstrong <- dt%>%
  dplyr::mutate(
  r1 = case_when( x.strong == 0 ~ rbinom(n(), 1, 0.16),
                  x.strong == 1 ~ rbinom(n(), 1, 0.06)),
  r2 = case_when( x.strong == 0 ~ rbinom(n(), 1, 0.47),
                  x.strong == 1 ~ rbinom(n(), 1, 0.18)),
  y.m1 = ifelse(r1==1 ,NA_integer_ ,y),
  y.m2 = ifelse(r2==1 ,NA_integer_ ,y)
    )

dt.mar.xweak <- dt%>%
  dplyr::mutate(
    r1 = case_when( x.weak == 0 ~ rbinom(n(), 1, 0.16),
                    x.weak == 1 ~ rbinom(n(), 1, 0.06)),
    r2 = case_when( x.weak == 0 ~ rbinom(n(), 1, 0.47),
                    x.weak == 1 ~ rbinom(n(), 1, 0.18)),
    y.m1 = ifelse(r1==1 ,NA_integer_ ,y),
    y.m2 = ifelse(r2==1 ,NA_integer_ ,y)
  )

dt.mnar <- dt%>%
  dplyr::mutate(
    nobs = n(),
    r = case_when(y == 1 ~ rbinom(nobs, 1, prob = do*n/nobs),
                  y == 0 ~ 0)
  )

dt.mnar <- dt%>%
  dplyr::filter(y == 1)%>%
  dplyr::mutate(r = rbinom(n(), 1, do*n/n()))%>%
  dplyr::bind_rows(dt%>%
                     dplyr::filter(y == 0)%>%
                     dplyr::mutate(r = 0))%>%
  dplyr::mutate(
    y.m = ifelse(r==1 ,NA_integer_ ,y)
    )


# count number of failures and sussesses
ym.sum <-
  dt.mcar%>%
    dplyr::filter(is.na(y.m) == F)%>%
    dplyr::group_by(y.m)%>%
    dplyr::summarise(nobs = n())%>%
    dplyr::mutate(y.name = ifelse(y.m==0, "fail", "success"))%>%
    dplyr::select(-y.m)%>%
    tidyr::spread('y.name', 'nobs')

#count number of incomplete observations

mi.mcar <- function(dt, seed){
  
  set.seed(111*seed)
  dt%>%
    dplyr::filter(is.na(y.m) == T)%>%
    dplyr::mutate(
      p.thresh = runif(n(), 0, 1),
      pstar = rbeta(n(), shape1 = ym.sum$success - 1, shape2 = ym.sum$fail - 1),
      y.im = ifelse(pstar>=p.thresh, 1, 0)
    )%>%
    dplyr::select(y.im)%>%
    dplyr::bind_rows(dt.mcar%>%
                       dplyr::filter(is.na(y.m) == F)%>%
                       dplyr::mutate(y.im = y)%>%
                       dplyr::select(y.im))%>%
    dplyr::summarise(phat = mean(y.im))
  
}
n.mi <- seq(1,10,1)

mi.est.sum <- map_df(n.mi, .f = mi.mcar, dt = dt.mcar, .id = 'n.mi')
