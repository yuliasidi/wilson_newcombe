plot_len <- function(dt){
  dt%>%
    ggplot(aes(x=mean_length,y=method)) +
    geom_point(aes(colour=factor(do_rate), shape=factor(n_obs)), size = 3) +
    facet_grid(pc~m2,labeller = label_both) + 
    theme_bw(base_size = 15) +
    labs( x = "Coverage rate",
          y = "Method",
          color = "Drop-out rate",
          shape = "Sample size") +
    scale_color_discrete(labels=c('10%', '30%')) + 
    theme(legend.position = 'bottom')
}
