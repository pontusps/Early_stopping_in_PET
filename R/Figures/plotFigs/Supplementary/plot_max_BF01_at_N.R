### Arguments
# path.plotdata, path to csv containing the data to be plotted

plot_max_BF01_at_N <- function(path.plotdata,title){
  
  maxBF01_at_N <- read.csv(file = path.plotdata)
  
  
  library(tidyverse)
  
  p1<-ggplot(data = maxBF01_at_N, aes( n_comparisons ,maxBF01_at_N)) + 
    geom_line() + 
    coord_cartesian(xlim = c(0,100),ylim = c(1,10) ) + 
    geom_hline(aes(yintercept = 4,color="BF>4" ), linetype="dashed") + 
    scale_color_manual(name = "", values = c("BF>4" = "#059af4")) + 
    xlab(label = 'N subjects/group') + 
    ylab(label = 'Maximum 1/BF (support for H0)') + 
    labs(title = title) + 
    scale_x_continuous(breaks = seq(0, 100, length.out = 11)) +
    scale_y_continuous(breaks = seq(0, 10, length.out = 11))+
    theme_bw() +
    theme(legend.position = "none",legend.title = element_blank() )
  
  p2<-ggplot(data = maxBF01_at_N, aes( n_comparisons ,perc_BF01_above_BF_threshold)) + 
    geom_line() + 
    coord_cartesian(xlim = c(0,100),ylim = c(0,100)) + 
    xlab(label = 'N subjects/group') + 
    ylab(label = '% BF in favor of H0') + 
    scale_x_continuous(breaks = seq(0, 100, length.out = 11)) +
    scale_y_continuous(breaks = seq(0, 100, length.out = 11))+
    theme_bw() 
  
  #Output
  plot.object <- list( max_possible_BF01 = p1, perc_BF01_support_H0 = p2)
  return(plot.object)
}
