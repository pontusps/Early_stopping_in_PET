####################################################
# Create FPR at different BF thresholds (k) Figure #
####################################################
### Pontus Plavén-Sigray (NRU) & Jonas Svensson, KI, August 2020

### Arguments
# path, path to data to be plotted
# title, title of plot (e.g., "Cross-sectional" or "Paired")
# start_n_comparisons, at what N BF sequential testing should start
# max_n_comparisons, Nmax comparisons 

plot_FPR_at_k <- function(path, title, start_n_comparisons, max_n_comparisons, element_text_size = NULL, legend.position = c(0.35, 0.8) ){
  
  df_to_plot <- read.csv(file = path)
  
    plot.out <-ggplot(data = df_to_plot,aes(x = maxN, y = FPR, group = k, color = k))+
      geom_line(size =2)+
      labs(title = title ,x="Nmax", y= "Rate of false positives") +
      theme_bw(base_size=30) +
      scale_color_manual(values=c("#00AFBB","#059af4", "#f2a950","black"))+
      scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                         labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
      theme(legend.position = legend.position)+
      labs(color = "Decision threshold")+
      coord_cartesian(ylim = c(0,0.15))
    
    if(!is.null(element_text_size) ){
      plot.out <- plot.out + theme( legend.text = element_text(size = element_text_size),
                                    legend.title = element_text(size = element_text_size) )
    }
    
    return(plot.out)
}
  
  