######################################
# BF threshold (k) to keep fpr at 5% #
######################################
### Pontus Plavén-Sigray (NRU) & Jonas Svensson, KI, August 2020

### Arguments
# path, path to data to be plotted
# start_n_comparisons, at what N BF sequential testing should start
# max_n_comparisons, Nmax comparisons 

plot_k_cntrl_fpr <- function(path, title, start_n_comparisons, max_n_comparisons, y_max_tick = NULL){

  df_to_plot <- read.csv(path)

  plot.out <- ggplot(data = df_to_plot,aes(x = maxN, y = k))+
    geom_line(size = 2)+
    labs(title = title ,x="Nmax", y= "Minimum BF threshold for FPR<5%") +
    scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                       labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
    theme(legend.title = element_blank()) + 
    theme_bw(base_size=30)
  
  if(!is.null(y_max_tick)){
    plot.out <- plot.out  +  
      scale_y_continuous(breaks = round(seq(0, y_max_tick, 1),1), labels = round(seq(0, y_max_tick, 1),1)) + 
      coord_cartesian(ylim = c(0,y_max_tick))
  }
  return(plot.out)

}

