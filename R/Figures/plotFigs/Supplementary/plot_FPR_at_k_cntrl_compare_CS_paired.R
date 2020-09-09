

### Arguments
# path, path to data to be plotted
# title, title of plot (e.g., "Cross-sectional" or "Paired")
# start_n_comparisons, at what N BF sequential testing should start
# max_n_comparisons, Nmax comparisons 


########################
# FPR AT DIFFERENT K 
########################

title <- ''
start_n_comparisons <- 3
max_n_comparisons <- 200
element_text_size = NULL
legend.position <- c(0.2,0.85)

path.fpr_at_k.CS <- './DerivedData/PlotData/fpr_at_different_k/Supplementary/CS_one_sided_stopH1_Nstart_3.csv'
path.fpr_at_k.paired <- './DerivedData/PlotData/fpr_at_different_k/Supplementary/Paired_one_sided_stopH1_Nstart_3.csv'
df_to_plot.fpr_at_k.CS <- read.csv(path.fpr_at_k.CS)
df_to_plot.fpr_at_k.paired <- read.csv(path.fpr_at_k.paired)

df_to_plot.fpr_at_k.CS$Design <- 'CS'
df_to_plot.fpr_at_k.paired$Design <- 'Paired'

df_to_plot.fpr_at_k <- rbind(df_to_plot.fpr_at_k.CS,df_to_plot.fpr_at_k.paired)

df_to_plot.fpr_at_k$Design <- as.factor(  df_to_plot.fpr_at_k$Design )

plot.out.fpr_at_k <-ggplot(data = df_to_plot.fpr_at_k,aes(x = maxN, y = FPR, color = k,linetype = Design ) )+
  geom_line(size =2 )+
  labs(title = title ,x="Nmax", y= "rate of false positives") +
  theme_bw(base_size=30) +
  scale_color_manual(values=c("#00AFBB","#059af4", "#f2a950","black"))+
  scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                     labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
  theme(legend.position = legend.position)+
  labs(color = "Decision threshold")+
  coord_cartesian(ylim = c(0,0.15)) #+ coord_cartesian(xlim = c(0,65))

plot.out.fpr_at_k.zoom <-ggplot(data = df_to_plot.fpr_at_k,aes(x = maxN, y = FPR, color = k,linetype = Design ) )+
  geom_line(size =2 )+
  labs(title = title ,x="Nmax", y= "rate of false positives") +
  theme_bw(base_size=30) +
  scale_color_manual(values=c("#00AFBB","#059af4", "#f2a950","black"))+
  scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                     labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
  theme(legend.position = legend.position)+
  labs(color = "Decision threshold")+
  coord_cartesian(ylim = c(0,0.15)) + 
  coord_cartesian(xlim = c(3,65))


########################
# K TO CONTROL FPR < 5% 
########################

title  <- ''
start_n_comparisons <- 3
max_n_comparisons <- 200
y_max_tick = NULL
legend.position <- c(0.2,0.85)

path.cntrl.CS <- './DerivedData/PlotData/k_cntrl_fpr/Supplementary/CS_stopH1_Nstart_3_one_sided.csv'
path.cntrl.paired <- './DerivedData/PlotData/k_cntrl_fpr/Supplementary/Paired_stopH1_Nstart_3_one_sided.csv'
df_to_plot.cntrl.CS <- read.csv(path.cntrl.CS)
df_to_plot.cntrl.paired <- read.csv(path.cntrl.paired)

df_to_plot.cntrl.CS$Design <- 'CS'
df_to_plot.cntrl.paired$Design <- 'Paired'

df_to_plot.cntrl <- rbind(df_to_plot.cntrl.CS,df_to_plot.cntrl.paired)

plot.out.cntrl_fpr <- ggplot(data = df_to_plot.cntrl,aes(x = maxN, y = k, col = Design, linetype = Design ))+
  geom_line(size = 2)+
  labs(title = title ,x="Nmax", y= "Minimum threshold for FPR<5%") +
  scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                     labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
  theme_bw(base_size=30)  + 
  theme(legend.title = element_blank(), legend.position = legend.position)  

plot.out.cntrl_fpr.zoom <- ggplot(data = df_to_plot.cntrl,aes(x = maxN, y = k, col = Design, linetype = Design ))+
  geom_line(size = 2)+
  labs(title = title ,x="Nmax", y= "Minimum threshold for FPR<5%") +
  scale_x_continuous(breaks = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6)), 
                     labels = round(seq(start_n_comparisons, max_n_comparisons, length.out = 6))) +
  theme_bw(base_size=30) + 
  coord_cartesian(xlim = c(3,65),ylim = c(0,12) ) + 
  theme(legend.title = element_blank(), legend.position = legend.position)  



fpr.plot <- cowplot::plot_grid(plot.out.fpr_at_k,plot.out.fpr_at_k.zoom,
                               plot.out.cntrl_fpr,plot.out.cntrl_fpr.zoom,
                               labels = c('', 'zoom','','zoom'), 
                               label_size = 25,ncol = 2)


cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S1_stopH1_Nstart_3_one_sided.png",
                   plot = fpr.plot,
                   base_width = 60,
                   base_height = 60,
                   dpi = 150,
                   units = 'cm')






