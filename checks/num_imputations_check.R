
x <- readRDS("results/p2_mars/p2_mar_strong_set_n6_m20.rds")
bin2mi::p2_eval(x, m2 = setting$m2[setting$set_n==6])


xo <- readRDS("results/p2_mars/p2_mar_strong_set_n6.rds")
bin2mi::p2_eval(xo, m2 = setting$m2[setting$set_n==6])
