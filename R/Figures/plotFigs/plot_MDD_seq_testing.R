########################################################################################################
### Sequential BF testing applied to CU medication free depressed patients v.s. healthy control data ###
########################################################################################################
# Pontus PS, NRU, June 2020

### NB: This public file reads in jittered data and will not produce the same results as presented in the article 

###Arguments
#path, path to data  MDD BF seq data 

plot_MDD_seq_testing <- function(path){
  
  plotdata.NM <- read.csv(path)
  
  plot.out<-ggplot(plotdata.NM,aes(Nsubject_group,BF,color = PrePostStop)) + 
    geom_line(size = 2,show.legend = F) + 
    scale_color_manual(values=c('#999999','black'))+
    coord_cartesian(xlim = c(10,40)) + 
    geom_hline(aes(yintercept= 1/k, linetype = "H0: 1/5"), colour= 'red',lty =2,size = 1.15)+
    geom_hline(aes(yintercept= k, linetype = "H1: 5"), colour= '#059af4',lty =2,size = 1.15)+
    geom_hline(yintercept = 1,colour= 'black',lty =2,size = 1.5) + 
    xlab('Number of Subjects per group') + 
    ylab("BF (log-scale)") +
    #scale_x_continuous(breaks = c(12,15,20,25,30,35,40)) + 
    scale_y_log10(breaks = c(1/5,1,5,50,500), labels = c("1/5","1","5","50", "500")) + 
    scale_linetype_manual(name = "Stopping thresholds", values = c(2, 2), 
                          guide = FALSE ) +
    theme_bw(base_size=30)  
  
  return(plot.out)
  
}