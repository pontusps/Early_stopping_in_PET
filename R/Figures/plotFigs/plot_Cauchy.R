####################################
# Create cauchy figure for article #
####################################
# Jonas Svensson (KI), Pontus Plavén-Sigray (NRU), August 2020

plot_Cauchy <- function(rscale = sqrt(2)/2, min_maxing = c(-3,3) ) {

library(tidyverse)

x <- seq(min_maxing[1], min_maxing[2], length=10000)

y <- dcauchy(x, scale = rscale)

plot_dens <- data.frame(x,y)
plot_dens$hypothesis <- "H1"


x <- rep(0, 10000)
y <- rep(0, 10000)

plot_dens2 <- data.frame(x,y)
plot_dens2$hypothesis <- "H0"

plot_dens <- rbind(plot_dens,plot_dens2)

density.plot <- 
  ggplot(data = plot_dens,aes(x = x, y = y, group = hypothesis, color = hypothesis))+
  geom_line(size =2)+
  scale_color_manual(values=c("red", "black"))+
  labs(title = "" ,x="Effect size (D)", y= "Density") +
  theme_bw(base_size=30)+
  geom_vline(mapping=aes(xintercept=0), color="red", size=2)+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.key.size = unit(2, "cm"))+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = c("-3","-2","-1","0","1","2","3"))

return(density.plot)
}
