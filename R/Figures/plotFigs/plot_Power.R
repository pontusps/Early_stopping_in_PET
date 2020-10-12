################################################
# "Power" graphs for seq-BF and fixed N NHST   #
################################################
### Pontus Plavén-Sigray (NRU) & Jonas Svensson, KI, August 2020

### Arguments
# path, path to csv containing plot data
# start_n_comparisons, at what N/group seq BF testing started
# max_n_comparisons, Nmax
# D_vector, vector of simulated effect sizes 
# print_FPR, print a text-box in figure reporting the FPR in numbers
# title, panel title
# ylab, y-axis label

plot_Power <- function(path, 
                       paired,
                       start_n_comparisons = start_n_comparisons,
                       max_n_comparisons = max_n_comparisons,
                       D_vector = D_vector,
                       print_FPR = F, 
                       title = '',
                       ylab = '', 
                       legpos1 = c(0.69, 0.4),
                       legpos2 = c(0.69, 0.15)){
  
  library(tidyverse)

  df_to_plot <- read.csv(file = path)
  
  #Specify text input to plot
  if (paired==TRUE) {
    CD <- "(Cohens Dz)"
  }else{
    CD <- "(Cohens D)"
  }
  
  x_lable <- paste0("Population effect ", CD)
  x_tick <-  D_vector
  
  if (!is.null(ylab)){
    y_lable = ylab
  }else{
    y_lable = "Stop decision (%)"
  }
  
  text_input <- list(p.title = title,
                     x_lable = x_lable,
                     x_tick = x_tick,
                     y_lable = y_lable)
  
  #Check for how many lines to draw and specify colors for them
  if(length(levels(df_to_plot$outcome))==2){
    color_input <- c( "#059af4", "black" )
  }else{
    color_input <-  c( "red","black","#059af4")
  }
  
  #########################
  # First plot: "power" curve
  #########################
  
  p1 <- ggplot(data = df_to_plot,aes(x = D, y = Decision, group = outcome, color = outcome))+
    geom_line(size=2)+
    scale_size_manual(values=c(1,2,2))+
    labs(title = text_input$p.title, x=text_input$x_lable, y=text_input$y_lable) +
    theme_bw(base_size=30) +
    scale_x_continuous(breaks = seq(0, D_vector[length(D_vector)], 
                                    length.out = 6), 
                       labels = seq(0, text_input$x_tick[length(text_input$x_tick)], length.out = 6)) +
    scale_y_continuous(breaks = seq(0, 100, length.out = 11), labels = seq(0, 100, length.out = 11)) +
    scale_color_manual(values=color_input)+
    theme(legend.title = element_blank(),legend.position = legpos1)+
    coord_cartesian(ylim=c(0,100))
  
  if (print_FPR == 1) {
  p1 <- p1 + 
    geom_label(aes(label = paste0("FPR: ", round(Decision[1],2),"%")), x = 0.2, y = 0, size = 8, color = "black", label.size = 1) 
  }
  
  #########################
  # Second plot: Average N
  #########################

  #Add NHST Average N (=max_n_comparisons) if not present and clean up df for second plot  
  df_to_plot <- df_to_plot %>%
    select(c(N, SD_ymax, SD_ymin, outcome, D ))
  
  if(length(levels(df_to_plot$outcome)) == 3){
    df_to_plot <- df_to_plot %>%
      filter(outcome == 'Stop for H1 OR H0') %>%
      mutate(outcome = 'BF seq testing')
    
    NHST <- data.frame(N = max_n_comparisons, 
                       SD_ymax= df_to_plot$SD_ymax, 
                       SD_ymin = df_to_plot$SD_ymin, 
                       outcome = 'fixed N approach', 
                       D = df_to_plot$D)
    
    df_to_plot <- rbind(df_to_plot,NHST)
    
  }
  
  #remove error bar above max_n_comparisons and below start_n_comparisons
  df_to_plot$SD_ymax[df_to_plot$SD_ymax>max_n_comparisons] <- max_n_comparisons
  df_to_plot$SD_ymin[df_to_plot$SD_ymin<start_n_comparisons] <- start_n_comparisons

  p2 <- ggplot(data = remove_missing(df_to_plot, na.rm = TRUE),aes(x = D, y = N, group = outcome, color = outcome))+
    geom_line(size =2)+
    labs(title = "" ,x=text_input$x_lable, y= "Average N per group") +
    theme_bw(base_size=30) +
    geom_ribbon(aes(ymax = SD_ymax, ymin = SD_ymin), 
                fill = "grey",alpha = .2, show.legend = FALSE) +
    theme(legend.title = element_blank(), legend.position = legpos2) +
    scale_color_manual(values=c("#059af4", "black"))+
    scale_x_continuous(breaks = seq(0, D_vector[length(D_vector)], length.out = 6), labels = seq(0, text_input$x_tick[length(text_input$x_tick)], length.out = 6)) +
    scale_y_continuous(breaks = seq(0, max_n_comparisons, length.out = 11), labels = seq(0, max_n_comparisons, length.out = 11))+
    coord_cartesian(ylim = c(0, max_n_comparisons))
  
  #Bind plots together
  plot.out<-list(power_panel = p1, N_panel = p2)
    
  return(plot.out)

}
