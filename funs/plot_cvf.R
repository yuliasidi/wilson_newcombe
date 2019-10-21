plot_cvf <- function(dt){
  dt%>%
    ggplot(aes(x=mean_cov,y=method)) +
    geom_point(aes(shape=factor(n_obs)), size = 3) +
    facet_grid(pc~m2,labeller = label_both) + 
    geom_vline(aes(xintercept=0.95),linetype=2) + 
    theme_bw(base_size = 15) +
    labs( x = "Coverage rate",
          y = "Method",
          shape = "Sample size") +
    theme(legend.position = 'bottom')
}
