plot_wid_paper <- function(dt){
  
  dt1 <- dt%>%
    dplyr::filter(method!="fm")%>%
    dplyr::mutate(plabel = sprintf('p[C]:%s', pc),
                  mlabel = sprintf('Delta:%s', m2))
  dt1%>%
    ggplot(aes(x=mean_length,y=method)) +
    geom_point(aes(colour=factor(do_rate), shape=factor(n_obs)), size = 3) +
    facet_grid(plabel~mlabel,labeller = ggplot2::label_parsed) + 
    scale_y_discrete(label = c("MI-Wald", "MI-NW", "MI-plug", "MI-Li"),
                     breaks = unique(dt1$method)) +
    theme_bw(base_size = 15) +
    labs( x = "Average width of the 95% confidence interval",
          y = "Method",
          color = "Drop-out rate",
          shape = "Sample size") +
    scale_color_discrete(labels=c('10%', '30%')) + 
    theme(legend.position = 'bottom')
}
