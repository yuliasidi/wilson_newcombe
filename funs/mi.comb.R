mi.comb <- function(dt, level=1, phat, var.phat){
 
  if (level==1){
    out <- dt%>%
      dplyr::summarise(qbar = mean(!!rlang::sym(phat)),
                ubar = mean(!!rlang::sym(var.phat)),
                b = var(!!rlang::sym(phat)),
                n = n())%>%
      dplyr::mutate(t = (1 + 1/n)*b + ubar,
                    rn = (1 + 1/n)*b/ubar,
                    v = case_when(b!= 0 ~ floor((n - 1)*(1 + ubar/((1+1/n)*b))^2),
                                  TRUE  ~ as.numeric(1000000000)))
    
  }
     
  if (level ==2){

    out.by.m <-
      dt%>%
      dplyr::group_by(m)%>%
      dplyr::summarise(qbar.m = mean(!!rlang::sym(phat)),
                ubar.m = mean(!!rlang::sym(var.phat)),
                q.m.var = var(!!rlang::sym(phat)),
                n = n())
    out <- 
      out.by.m%>%
      dplyr::summarise(qbar = mean(qbar.m),
                       ubar = mean(ubar.m),
                       b = var(qbar.m),
                       w = mean(q.m.var),
                       n = mean(n),
                       m = n())%>%
      dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                    rn = (1 + 1/n)*b/ubar,
                    sn = (1 - 1/n)*w/ubar,
                    v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                    v = case_when(b!=0 ~ floor(1/v_1),
                                  TRUE ~ as.numeric(1000000000)))
  }

  return(out)  
}
