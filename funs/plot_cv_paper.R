plot_cv_paper <- function(dt, lim = c(0.89, 1), bks = c(0.9, 0.93, 0.95, 0.97, 1)){
  
  dt1 <- dt%>%
    dplyr::filter(method!="fm")%>%
    dplyr::mutate(plabel = sprintf('p[C]:%s', pc),
                  mlabel = sprintf('Delta:%s', m2))
  
  plot <- 
    dt1%>%
    ggplot(aes(x=mean_cov,y=method)) +
    geom_point(aes(colour=factor(do_rate), shape=factor(n_obs)), size = 3) +
    facet_grid(plabel~mlabel,labeller = ggplot2::label_parsed) + 
    geom_vline(aes(xintercept=0.95),linetype=2) + 
    geom_vline(aes(xintercept=0.9), linetype=3) + 
    scale_y_discrete(label = c("MI-Wald", "MI-NW", "MI-plug", "MI-Li"),
                     breaks = unique(dt1$method)) +
    scale_x_continuous(limits = lim, breaks = bks) + 
    theme_bw(base_size = 15) +
    labs( x = "Coverage probability",
          y = "Method",
          color = "Drop-out rate",
          shape = "Sample size") +
    scale_color_discrete(labels=c('10%', '30%')) + 
    theme(legend.position = 'bottom')
  
  return(plot)
  
}
