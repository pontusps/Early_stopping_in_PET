#####################################
#Create spaghetthi plot for article #
#####################################
# Jonas Svensson (KI), Pontus Plavén-Sigray (NRU), August 2020

### Arguments: 
#path.data,full path to data for spaghetti-plot 

plot_Spaghetti <- function(path.data) {

library(tidyverse)

#Read example data
BF.df <-read.csv(file = path.data)

plot.out <- ggplot(data = BF.df,aes(x = subject_no, y = BF_log, group = study_no, color = stop))+
  geom_line(size = 2)+
  labs(title = "" ,x="Number of subjects per group", y= "BF (log-scale)") +
  scale_color_manual(values=c("red", "#059af4", "black"))+
  geom_hline(color = "#059af4", yintercept = 4, lty =2, size = 1.2)+
  geom_hline(color = "red", yintercept = -4, lty =2, size = 1.2)+
  geom_hline(color = "black", yintercept = 0, lty =2, size = 1.5)+
  theme_bw(base_size=30) +
  coord_cartesian(ylim=c(-5,5))+
  scale_x_continuous(breaks = seq(0, 30, by=5), labels = seq(0, 30, by=5)) +
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.75))+
  scale_y_continuous(breaks = c(-4,0,4), labels = c("1/4","1","4"))

return(plot.out)

}



