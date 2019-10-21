setting <- readRDS("setting.rds")
nn <- 16
tt <- readRDS(sprintf("results/p2_mnar_ign/p2_mnar_ign_set_n%s.rds", nn))
tt <- readRDS(sprintf("results/p2_mnar/p2_mnar_set_n%s.rds", nn))

pc<-setting$pc[setting$set_n==nn]
pt<-setting$pc[setting$set_n==nn] - setting$m2[setting$set_n==nn]


  tt%>%purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
  dplyr::filter(method%in%c('wn-mi for pc'))%>%
  dplyr::mutate(coverage = ifelse(pc<=upper_bound & pc>=lower_bound, 1, 0))%>%
  summarise(mean(coverage), mean(qbar))
  
    tt%>%purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method%in%c('wn-mi for pt'))%>%
    dplyr::mutate(coverage = ifelse(pt<=upper_bound & pt>=lower_bound, 1, 0))%>%
    summarise(mean(coverage), mean(qbar))


bin2mi::p2_eval(tt, m2 = setting$m2[setting$set_n==nn])


setting%>%filter(set_n == nn)


cc <- seq(1,16,1)

purrr::map(cc, .f = function(x){
  dt <- readRDS(sprintf("results/p2_mnar/p2_mnar_set_n%s.rds", x))
  
  purrr::discard(dt, .p=function(x0) is.character(x0[[1]]))%>%length()
  
} )


purrr::map(cc, .f = function(x){
  dt <- readRDS(sprintf("results/p2_mnar/p2_mnar_set_n%s.rds", x))
  
  purrr::discard(dt, .p=function(x0) is.character(x0[[1]]))%>%length()
  
} )
