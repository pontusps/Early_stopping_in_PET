#############################################################################
### Code to reproduce figures in "Early stopping in clinical PET studies" ###
#############################################################################
### Pontus Plavén-Sigray, NRU, August 2020

#NB1: output from 'BF_seq_sim_parallel' is not saved on https://github.com/pontusps/Early_stopping_in_PET. Files are to large. 
#NB2: public data for Figure 6 is jittered and will not reproduce the exact same plot as seen in the article. 

#Make sure to setwd to parent folder of project, e.g.: 
#setwd('/Users/thomas.bayes/Documents/Github/Early_stopping_in_PET')

library(tidyverse)


###  #####################  ###
###  ### SIMULATE DATA ###  ###
###  #####################  ###
# Simulate data and run seq BF testing. Takes time and creates large files. 

source('./R/Simulations/BF_seq_sim_parallel.R')

####################################
### Simulate D = 0, 100.000 trials #
####################################

#Creates cross-sectional two-sided sim-file in "./DerivedData/SimulationData"
BF_sim_out <- BF_seq_sim_parallel(paired = F,
                                  n_trials = 100000,
                                  alternative = 'two.sided',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 100,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/','BFsim_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

#Creates Cross-sectional one-sided test sim-files in"./DerivedData/SimulationData"
BF_sim_out <- BF_seq_sim_parallel(paired = F,
                                  n_trials = 100000,
                                  alternative = 'positive',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 100,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/CrossSectional/OneSided/Enlarged_BFsim_D_0/','BFsim_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

#Creates paired two-sided sim-file in "./DerivedData/SimulationData"
BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                  n_trials = 100000,
                                  alternative = 'two.sided',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 100,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/TwoSided/Enlarged_BFsim_D_0/','BFsim_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

#Creates paired one-sided sim-file in "./DerivedData/SimulationData"
BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                  n_trials = 100000,
                                  alternative = 'positive',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 100,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/OneSided/Enlarged_BFsim_D_0/','BFsim_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

################################################
### Simulate D = 0 to D = 1.5, 30.000 trials ###
################################################

#Create vector with population effect sizes to simulate
D_vector<-seq(0,1.5,0.05) 

#Creates cross-sectional two-sided sim-files in "./DerivedData/SimulationData"
for(d in D_vector){
BF_sim_out <- BF_seq_sim_parallel(paired = F, 
                                  n_trials = 30000,
                                  alternative = 'two.sided',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 100,
                                  ratio_null_true = 0,
                                  mean_diff = d,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/CrossSectional/TwoSided/','BFsim_D_',D_vector[d],'.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

}

#Creates cross-sectional one-sided sim-files in "./DerivedData/SimulationData"
for(d in D_vector){
  BF_sim_out <- BF_seq_sim_parallel(paired = F,
                                    n_trials = 30000, 
                                    alternative = 'positive',
                                    rscale = sqrt(2)/2,
                                    n_comparisons = 100,
                                    ratio_null_true = 0,
                                    mean_diff = d, 
                                    standard_dev = 1,
                                    cpus = 4)
  
  saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/CrossSectional/OneSided/','BFsim_D_',D_vector[d],'.rds'))
  rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out
}

#Creates paired two-sided sim-files in "./DerivedData/SimulationData"
for(d in D_vector){
  BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                    n_trials = 30000,
                                    alternative = 'two.sided',
                                    rscale = sqrt(2)/2,
                                    n_comparisons = 100,
                                    ratio_null_true = 0,
                                    mean_diff = d,
                                    standard_dev = 1,
                                    cpus = 4)
  
  saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/TwoSided/','BFsim_D_',D_vector[d],'.rds'))
  rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out
}

#Creates paired one-sided sim-files in "./DerivedData/SimulationData"
for(d in D_vector){
  BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                    n_trials = 30000,
                                    alternative = 'positive',
                                    rscale = sqrt(2)/2,
                                    n_comparisons = 100,
                                    ratio_null_true = 0,
                                    mean_diff = d,
                                    standard_dev = 1,
                                    cpus = 4)
  
  saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/OneSided/','BFsim_D_',D_vector[d],'.rds'))
  rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out
}

###  ################################  ###
###  ### SLIM DOWN SIMULATED DATA ###  ###
###  ################################  ###
# Makes the data manageable and shareable

source('./R/Slimmer/sim_slimmer.R')

#################################
### For D = 0, 100.000 trials ###
#################################

#Slim cross-sectional two-sided sim-file and save
files_to_slim <- paste0('./DerivedData/SimulationData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/BFsim_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 100000, 
                        n_comparisons = 100, 
                        paired = F, 
                        alternative = 'two.sided', 
                        decimals = 2)

saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim cross-sectional one-sided sim-file and save
files_to_slim <- paste0('./DerivedData/SimulationData/CrossSectional/OneSided/Enlarged_BFsim_D_0/BFsim_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 100000, 
                        n_comparisons = 100, 
                        paired = F, 
                        alternative = 'positive', 
                        decimals = 2)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/CrossSectional/OneSided/Enlarged_BFsim_D_0/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim paired two-sided sim-file and save
files_to_slim <- paste0('./DerivedData/SimulationData/Paired/TwoSided/Enlarged_BFsim_D_0/BFsim_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 100000, 
                        n_comparisons = 100, 
                        paired = T, 
                        alternative = 'two.sided', 
                        decimals = 2)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/TwoSided/Enlarged_BFsim_D_0/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim paired one-sided sim-file and save
files_to_slim <- paste0('./DerivedData/SimulationData/Paired/OneSided/Enlarged_BFsim_D_0/BFsim_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 100000, 
                        n_comparisons = 100, 
                        paired = T,
                        alternative = 'positive', 
                        decimals = 2)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/OneSided/Enlarged_BFsim_D_0/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

###########################################
### For D = 0 to D = 1.5, 30.000 trials ###
###########################################

#Create vector with population effect sizes to simulate
D_vector<-seq(0,1.5,0.05) 

#Slim cross-sectional two-sided sim-files and save
files_to_slim <- paste0('./DerivedData/SimulationData/CrossSectional/TwoSided/BFsim_D_',D_vector,'.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim,
                        n_trials = 15000, 
                        n_comparisons = 100, 
                        paired = F, 
                        alternative = 'two.sided', 
                        decimals = 1)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/CrossSectional/TwoSided/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim cross-sectional one-sided sim-files and save
files_to_slim <- paste0('./DerivedData/SimulationData/CrossSectional/OneSided/BFsim_D_',D_vector,'.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 15000, 
                        n_comparisons = 100, 
                        paired = F, 
                        alternative = 'positive', 
                        decimals = 1)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/CrossSectional/OneSided/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim paired two-sided sim-files and save
files_to_slim <- paste0('./DerivedData/SimulationData/Paired/TwoSided/BFsim_D_',D_vector,'.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 15000, 
                        n_comparisons = 100, 
                        paired = T, 
                        alternative = 'two.sided', 
                        decimals = 1)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/TwoSided/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

#Slim paired one-sided sim-files and save 
files_to_slim <- paste0('./DerivedData/SimulationData/Paired/OneSided/BFsim_D_',D_vector,'.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 15000, 
                        n_comparisons = 100, 
                        paired = T, 
                        alternative = 'positive', 
                        decimals = 1)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/OneSided/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

###  ###########################  ###
###  ### CREATE DATA TO PLOT ###  ###
###  ###########################  ###

################################################################
# FPR at different BF thresholds (k): Stop only for H1, not H0 #
################################################################

source('./R/Figures/getPlotData/getPlotData_fpr_at_k.R')

# cross-sectional two-sided 
fpr_at_k.df <- getplotData_fpr_at_k(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                    start_n_comparisons = 12, 
                                    max_n_comparisons = 50, 
                                    k_BF10 = c(3,4,6), 
                                    k_BF01 = c(Inf,Inf,Inf),  #stop only for H1
                                    decimals = 2)

write.csv(x = fpr_at_k.df,file = './DerivedData/PlotData/fpr_at_different_k/CS_two_sided_stopH1.csv',row.names = F)
rm(list = c("fpr_at_k.df")) #clean enviroment of fpr_at_k.df

# paired two-sided 
fpr_at_k.df <- getplotData_fpr_at_k(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                    start_n_comparisons = 12, 
                                    max_n_comparisons = 50, 
                                    k_BF10 = c(3,4,6), 
                                    k_BF01 = c(Inf,Inf,Inf),  #stop only for H1
                                    decimals = 2)

write.csv(x = fpr_at_k.df,file = './DerivedData/PlotData/fpr_at_different_k/Paired_two_sided_stopH1.csv',row.names = F)
rm(list = c("fpr_at_k.df")) #clean enviroment of fpr_at_k.df

#########################################
# BF threshold (k) to control FPR at 5% #
#########################################

source('./R/Figures/getPlotData/getPlotData_k_cntrl_fpr.R')

### Stop only for H1, not H0

# cross-sectional two-sided, Nstart = 12, Nmax = 50
k_fpr_5perc.df <- getPlotData_k_cntrl_fpr(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                          start_n_comparisons = 12, 
                                          max_n_comparisons = 50, 
                                          start_BF10 = 1.5, 
                                          start_BF01 = Inf,  #stop only for H1
                                          fpr_limit = 0.05, 
                                          step_size = 0.05, 
                                          decimals = 2)

write.csv(x = k_fpr_5perc.df,file = './DerivedData/PlotData/k_cntrl_fpr/CS_stopH1_two_sided.csv',row.names = F)
rm(list = c("k_fpr_5perc.df")) #clean enviroment of k_fpr_5perc.df

# paired two-sided, Nstart = 12, Nmax = 50
k_fpr_5perc.df <- getPlotData_k_cntrl_fpr(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                          start_n_comparisons = 12, 
                                          max_n_comparisons = 50, 
                                          start_BF10 = 1.5, 
                                          start_BF01 = Inf,  #stop only for H1
                                          fpr_limit = 0.05, 
                                          step_size = 0.05, 
                                          decimals = 2)

write.csv(x = k_fpr_5perc.df,file = './DerivedData/PlotData/k_cntrl_fpr/Paired_stopH1_two_sided.csv',row.names = F)
rm(list = c("k_fpr_5perc.df")) #clean enviroment of k_fpr_5perc.df

################################################
# BF and NHST "power"-curves: Stop only for H1 #
################################################

source('./R/Figures/getPlotData/getPlotData_power_curves.R')

### Stopping only for H1, not H0

# cross-sectional two-sided, Nstart = 12, Nmax = 30
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 30,
                                    test_seq = 1,
                                    k_BF10 = 4.05,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/CS_two_sided_stopH1.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df


# paired two-sided, Nstart = 12, Nmax = 30
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 30,
                                    test_seq = 1,
                                    k_BF10 = 4.25,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Paired_two_sided_stopH1.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

#########################################
# BF "power"-curves: Stop for H1 and H0 #
#########################################

# cross-sectional one-sided, Nstart = 12, Nmax = 30
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 30,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/CS_one_sided_stopH1_stopH0.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired one-sided, Nstart = 12, Nmax = 30
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 30,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Paired_one_sided_stopH1_stopH0.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

#######################################################
# seq BF testing applied to MDD patients v.s. controls 
#######################################################

source('./R/Figures/getPlotData/getPlotData_MDD_seq_testing.R')

#NB: This public file reads in jittered data and will not produce the same results as presented in the article 

MDD_BF_seq.df <- getPlotData_MDD_seq_testing(path = './RawData/MDD_WAY_data/MDD_NoMed_way_tidy_for_seq_test_jittered.csv')
write.csv(x = MDD_BF_seq.df,file = './DerivedData/PlotData/MDD_seq_test/BF_seq_MDD_WAY_jittered.csv',row.names = F)

###  #################  ###
###  ### PLOT DATA ###  ###
###  #################  ###

###########################################################
# Figure 1: Cauchy plot - example of specifying H1 and H0 #
###########################################################

source('./R/Figures/plotFigs/plot_Cauchy.R')

cauchy.plot <- plot_Cauchy(rscale = sqrt(2)/2, min_maxing = c(-3,3))
cowplot::save_plot(filename = "./Results/Figures/Figure1.png",
                   plot = cauchy.plot,
                   base_width = 50,
                   base_height = 30,
                   units = 'cm')

##########################################################
# Figure 2: Spaghetti plot - examples of BF trajectories #
##########################################################

source('./R/Figures/plotFigs/plot_Spaghetti.R')

spaghetti.plot <- plot_Spaghetti(path.data = './DerivedData/PlotData/spaghetti/BF_for_spagetthi.csv')
cowplot::save_plot(filename = "./Results/Figures/Figure2.png",
                   plot = spaghetti.plot,
                   base_width = 50,
                   base_height = 30,
                   units = 'cm')

#############################
# Figure 3: False positives #
#############################

### Fig 3 A-B: False positives at three different k

source('./R/Figures/plotFigs/plot_FPR_at_k.R')

fpr_k_CS <- plot_FPR_at_k(path = './DerivedData/PlotData/fpr_at_different_k/CS_two_sided_stopH1.csv', 
                     title = 'Cross-Sectional', 
                     start_n_comparisons = 12, 
                     max_n_comparisons = 50)

fpr_k_paired <- plot_FPR_at_k(path = './DerivedData/PlotData/fpr_at_different_k/Paired_two_sided_stopH1.csv', 
                         title = 'Paired', 
                         start_n_comparisons = 12, 
                         max_n_comparisons = 50)

### Figure 3 C-D: k needed to keep false positive < 5% at different Nmax 

source('./R/Figures/plotFigs/plot_k_cntrl_fpr.R')

k_cntrl_fpr_CS <- plot_k_cntrl_fpr(path = './DerivedData/PlotData/k_cntrl_fpr/CS_stopH1_two_sided.csv', 
                         title = '', 
                         start_n_comparisons = 12, 
                         max_n_comparisons = 50,
                         y_max_tick = 6)

k_cntrl_fpr_paired <- plot_k_cntrl_fpr(path = './DerivedData/PlotData/k_cntrl_fpr/Paired_stopH1_two_sided.csv', 
                         title = '', 
                         start_n_comparisons = 12, 
                         max_n_comparisons = 50,
                         y_max_tick = 6)

### Bind and save Figure 3

fpr.plot <- cowplot::plot_grid(fpr_k_CS,fpr_k_paired,k_cntrl_fpr_CS,k_cntrl_fpr_paired,
                               labels = c('A', 'B','C','D'), 
                               label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Figure3.png",
                   plot = fpr.plot,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#######################################################
# Figure 4: BF and NHST power curves: Stop only for H1 #
#######################################################

source('./R/Figures/plotFigs/plot_Power.R')

#Cross-sectional
BF_NHST_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1/CS_two_sided_stopH1.csv', 
                            paired = F,
                            start_n_comparisons = 12,
                            max_n_comparisons = 30,
                            D_vector = D_vector,
                            print_FPR = F, 
                            title = 'Cross-sectional',
                            ylab='Positive findings (%)', 
                            legpos1 = c(0.69, 0.4),
                            legpos2 = c(0.69, 0.15))

#paired
BF_NHST_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Paired_two_sided_stopH1.csv', 
                                 paired = T,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 30,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Paired',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.15))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                     labels = c('A', 'B', 'C','D'), 
                     label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Figure4.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')


#####################################################
# Figure 5: BF power curves: Stop for H1 and for H0 #
#####################################################

#cross-sectional
BF_power_N_stopH0_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/CS_one_sided_stopH1_stopH0.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 30,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Stop decisions (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.15))
#paired
BF_power_N_stopH0_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Paired_one_sided_stopH1_stopH0.csv', 
                                   paired = T,
                                   start_n_comparisons = 12,
                                   max_n_comparisons = 30,
                                   D_vector = D_vector,
                                   print_FPR = F, 
                                   title = 'Paired',
                                   ylab='Stop decisions (%)', 
                                   legpos1 = c(0.69, 0.4),
                                   legpos2 = c(0.69, 0.15))

BF_stopH0_power <- plot_grid(BF_power_N_stopH0_CS$power_panel, BF_power_N_stopH0_paired$power_panel,
                           BF_power_N_stopH0_CS$N_panel,BF_power_N_stopH0_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)


cowplot::save_plot(filename = "./Results/Figures/Figure5.png",
                   plot = BF_stopH0_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#######################################################################
# Figure 6: BF sequential testing of [11]WAY MDD patients v.s. controls
#######################################################################

source('./R/Figures/plotFigs/plot_MDD_seq_testing.R')

#Jittered data
MDD_seq_test_jittered <- plot_MDD_seq_testing(path = './DerivedData/PlotData/MDD_seq_test/BF_seq_MDD_WAY_jittered.csv')
cowplot::save_plot(filename = "./Results/Figures/Figure6jittered.png",
                   plot = MDD_seq_test_jittered,
                   base_width = 40,
                   base_height = 20,
                   units = 'cm')

#Real data
MDD_seq_test <- plot_MDD_seq_testing(path = './DerivedData/PlotData/MDD_seq_test/BF_seq_MDD_WAY.csv')
cowplot::save_plot(filename = "./Results/Figures/Figure6.png",
                   plot = MDD_seq_test,
                   base_width = 40,
                   base_height = 20,
                   units = 'cm')


###  #############################  ###
###  ### Supplementary Figures ###  ###
###  #############################  ###

#################
### SFigure 1 ###
#################
### FPR at different k (A-B) and k to keep fpr <5% (C-D) when stopping for both H1 and H0, one-sided, Nmax = 200

#### Create simulated data 

BF_sim_out <- BF_seq_sim_parallel(paired = F,
                                  n_trials = 50000,
                                  alternative = 'positive',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 200,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/CrossSectional/OneSided/Supplementary/','BFsim_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

#Creates paired two-sided sim-file in "./DerivedData/SimulationData"
BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                  n_trials = 50000,
                                  alternative = 'positive',
                                  rscale = sqrt(2)/2,
                                  n_comparisons = 200,
                                  ratio_null_true = 1,
                                  mean_diff = 0,
                                  standard_dev = 1,
                                  cpus = 4)

saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/OneSided/Supplementary/','BFsim_Nmax_200_D_0.rds'))
rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out

### Slim data
files_to_slim <- paste0('./DerivedData/SimulationData/CrossSectional/OneSided/Supplementary/BFsim_Nmax_200_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 50000, 
                        n_comparisons = 200, 
                        paired = F, 
                        alternative = 'positive', 
                        decimals = 2)

saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/CrossSectional/OneSided/Supplementary/','BFsim_Nmax_200.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

files_to_slim <- paste0('./DerivedData/SimulationData/Paired/OneSided/Supplementary/BFsim_Nmax_200_D_0.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 50000, 
                        n_comparisons = 200, 
                        paired = T, 
                        alternative = 'positive', 
                        decimals = 2)

saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/OneSided/Supplementary/','BFsim_Nmax_200.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

### Get plot-data

# cross-sectional one-sided 
fpr_at_k.df <- getplotData_fpr_at_k(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/Supplementary/BFsim_Nmax_200.rds',
                                    start_n_comparisons = 12, 
                                    max_n_comparisons = 200, 
                                    k_BF10 = c(3,4,6), 
                                    k_BF01 = c(3,4,6), 
                                    decimals = 2)

write.csv(x = fpr_at_k.df,file = './DerivedData/PlotData/fpr_at_different_k/Supplementary/CS_one_sided_stopH1_stopH0.csv',row.names = F)

# paired one-sided 
fpr_at_k.df <- getplotData_fpr_at_k(path.slim = './DerivedData/SlimmedData/Paired/OneSided/Supplementary/BFsim_Nmax_200.rds',
                                    start_n_comparisons = 12, 
                                    max_n_comparisons = 200, 
                                    k_BF10 = c(3,4,6), 
                                    k_BF01 = c(3,4,6), 
                                    decimals = 2)

write.csv(x = fpr_at_k.df,file = './DerivedData/PlotData/fpr_at_different_k/Supplementary/Paired_one_sided_stopH1_stopH0.csv',row.names = F)
rm(list = c("fpr_at_k.df")) #clean enviroment of fpr_at_k.df

# cross-sectional one-sided
k_fpr_5perc.df <- getPlotData_k_cntrl_fpr(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/Supplementary/BFsim_Nmax_200.rds',
                                          start_n_comparisons = 12, 
                                          max_n_comparisons = 200, 
                                          start_BF10 = 1.5, 
                                          start_BF01 = 1.5, 
                                          fpr_limit = 0.05, 
                                          step_size = 0.05, 
                                          decimals = 2)

write.csv(x = k_fpr_5perc.df,file = './DerivedData/PlotData/k_cntrl_fpr/Supplementary/CS_stopH1_stopH0_one_sided.csv',row.names = F)
rm(list = c("k_fpr_5perc.df")) #clean enviroment of k_fpr_5perc.df

# paired one-sided
k_fpr_5perc.df <- getPlotData_k_cntrl_fpr(path.slim = './DerivedData/SlimmedData/Paired/OneSided/Supplementary/BFsim_Nmax_200.rds',
                                          start_n_comparisons = 12, 
                                          max_n_comparisons = 200, 
                                          start_BF10 = 1.5, 
                                          start_BF01 = 1.5, 
                                          fpr_limit = 0.05, 
                                          step_size = 0.05, 
                                          decimals = 2)

write.csv(x = k_fpr_5perc.df,file = './DerivedData/PlotData/k_cntrl_fpr/Supplementary/Paired_stopH1_stopH0_one_sided.csv',row.names = F)
rm(list = c("k_fpr_5perc.df")) #clean enviroment of k_fpr_5perc.df

### Plot data

fpr_k_CS <- plot_FPR_at_k(path = './DerivedData/PlotData/fpr_at_different_k/Supplementary/CS_one_sided_stopH1_stopH0.csv', 
                          title = 'Cross-Sectional', 
                          start_n_comparisons = 12, 
                          max_n_comparisons = 200,
                          element_text_size = 21, 
                          legend.position = c(0.25, 0.82) )

fpr_k_paired <- plot_FPR_at_k(path = './DerivedData/PlotData/fpr_at_different_k/Supplementary/Paired_one_sided_stopH1_stopH0.csv', 
                              title = 'Paired', 
                              start_n_comparisons = 12, 
                              max_n_comparisons = 200,
                              element_text_size = 21, 
                              legend.position = c(0.25, 0.82) )


k_cntrl_fpr_CS <- plot_k_cntrl_fpr(path = './DerivedData/PlotData/k_cntrl_fpr/Supplementary/CS_stopH1_stopH0_one_sided.csv', 
                                   title = '', 
                                   start_n_comparisons = 12, 
                                   max_n_comparisons = 200,
                                   y_max_tick = 8)

k_cntrl_fpr_paired <- plot_k_cntrl_fpr(path = './DerivedData/PlotData/k_cntrl_fpr/Supplementary/Paired_stopH1_stopH0_one_sided.csv', 
                                       title = '', 
                                       start_n_comparisons = 12, 
                                       max_n_comparisons = 200,
                                       y_max_tick = 8)

fpr.plot <- cowplot::plot_grid(fpr_k_CS,fpr_k_paired,k_cntrl_fpr_CS,k_cntrl_fpr_paired,
                               labels = c('A', 'B','C','D'), 
                               label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S1.png",
                   plot = fpr.plot,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')


#################
### SFigure 2 ###
#################
#0-centered Cauchy seq-BF and seq-NHST being identical when setting fpr to 5% and stopping only for H0.  

source('./R/Figures/plotFigs/plot_Power_seqNHST.R')

#Cross-sectional
BF_NHST_power_N_CS <- plot_Power_seqNHST(path = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_seqNHST.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 30,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.35),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_NHST_power_N_paired <- plot_Power_seqNHST(path = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_seqNHST.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 30,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.35),
                                     legpos2 = c(0.69, 0.15))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S2.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')




#################
### SFigure 3 ###
#################
#Power curve BF and NHST, Nmax = 15

### Get plot data

# cross-sectional two-sided, Nstart = 12, Nmax = 15
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 15,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax15.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired two-sided, Nstart = 8, Nmax = 15
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 15,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax15.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure

#Cross-sectional
BF_NHST_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax15.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 15,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.15),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_NHST_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax15.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 15,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.15),
                                     legpos2 = c(0.69, 0.15))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S3.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 4 ###
#################
#Power curve BF and NHST, Nmax = 20

### Get plot data

# cross-sectional two-sided, Nstart = 12, Nmax = 20
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 20,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax20.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired two-sided, Nstart = 12, Nmax = 20
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 20,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax20.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure


#Cross-sectional
BF_NHST_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax20.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 20,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.15),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_NHST_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax20.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 20,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.15),
                                     legpos2 = c(0.69, 0.15))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S4.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 5 ###
#################
#Power curve BF and NHST, Nmax = 50

### Get plot data

# cross-sectional two-sided, Nstart = 12, Nmax = 50
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 50,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax50.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired two-sided, Nstart = 12, Nmax = 50
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 50,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax50.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure


#Cross-sectional
BF_NHST_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax50.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 50,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_NHST_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax50.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 50,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.4),
                                     legpos2 = c(0.69, 0.15))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S5.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 6 ###
#################
#Power curve BF and NHST, Nmax = 100

### Get plot data

# cross-sectional two-sided, Nstart = 12, Nmax = 100
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 100,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax100.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired two-sided, Nstart = 12, Nmax = 100
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/BFsims.rds',
                                    alternative = 'two.sided',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 100,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = Inf,
                                    decimals = 1,
                                    NHST_curve = T,
                                    sig.level = 0.05)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax100.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure

#Cross-sectional
BF_NHST_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/CS_two_sided_stopH1_Nmax100.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 100,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.62))

#paired
BF_NHST_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1/Supplementary/Paired_two_sided_stopH1_Nmax100.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 100,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.4),
                                     legpos2 = c(0.69, 0.62))

BF_NHST_power <- plot_grid(BF_NHST_power_N_CS$power_panel, BF_NHST_power_N_paired$power_panel,
                           BF_NHST_power_N_CS$N_panel,BF_NHST_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S6.png",
                   plot = BF_NHST_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')



#################
### SFigure 7 ###
#################
#Power curve BF stop for H1 and H0, Nmax = 15

### Get plot data

# cross-sectional one-sided, Nstart = 12, Nmax = 15
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 15,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax15.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired one-sided, Nstart = 12, Nmax = 15
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 15,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax15.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure


#Cross-sectional
BF_stopH0_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax15.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 15,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.21),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_stopH0_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax15.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 15,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.21),
                                     legpos2 = c(0.69, 0.15))

BF_stopH0_power <- plot_grid(BF_stopH0_power_N_CS$power_panel, BF_stopH0_power_N_paired$power_panel,
                           BF_stopH0_power_N_CS$N_panel,BF_stopH0_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S7.png",
                   plot = BF_stopH0_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 8 ###
#################
#Power curve BF stop for H1 and H0, Nmax = 20

### Get plot data

# cross-sectional one-sided, Nstart = 12, Nmax = 20
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 20,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax20.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired one-sided, Nstart = 12, Nmax = 20
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 20,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax20.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure


#Cross-sectional
BF_stopH0_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax20.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 20,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.3),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_stopH0_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax20.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 20,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.3),
                                     legpos2 = c(0.69, 0.15))

BF_stopH0_power <- plot_grid(BF_stopH0_power_N_CS$power_panel, BF_stopH0_power_N_paired$power_panel,
                           BF_stopH0_power_N_CS$N_panel,BF_stopH0_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S8.png",
                   plot = BF_stopH0_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 9 ###
#################
#Power curve BF stop for H1 and H0, Nmax = 50

### Get plot data

# cross-sectional one-sided, Nstart = 12, Nmax = 50
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 50,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax50.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired one-sided, Nstart = 12, Nmax = 50
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 50,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax50.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure


#Cross-sectional
BF_stopH0_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax50.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 50,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.15))

#paired
BF_stopH0_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax50.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 50,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.4),
                                     legpos2 = c(0.69, 0.15))

BF_stopH0_power <- plot_grid(BF_stopH0_power_N_CS$power_panel, BF_stopH0_power_N_paired$power_panel,
                           BF_stopH0_power_N_CS$N_panel,BF_stopH0_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S9.png",
                   plot = BF_stopH0_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 10 ###
#################
#Power curve BF stop for H1 and H0, Nmax = 100

### Get plot data

# cross-sectional one-sided, Nstart = 12, Nmax = 100
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/CrossSectional/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = F,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 100,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax100.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

# paired one-sided, Nstart = 12, Nmax = 100
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 100,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)


write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax100.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df

### Plot figure

#Cross-sectional
BF_stopH0_power_N_CS <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/CS_one_sided_stopH1_Nmax100.csv', 
                                 paired = F,
                                 start_n_comparisons = 12,
                                 max_n_comparisons = 100,
                                 D_vector = D_vector,
                                 print_FPR = F, 
                                 title = 'Cross-sectional',
                                 ylab='Positive findings (%)', 
                                 legpos1 = c(0.69, 0.4),
                                 legpos2 = c(0.69, 0.6))

#paired
BF_stopH0_power_N_paired <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Paired_one_sided_stopH1_Nmax100.csv', 
                                     paired = T,
                                     start_n_comparisons = 12,
                                     max_n_comparisons = 100,
                                     D_vector = D_vector,
                                     print_FPR = F, 
                                     title = 'Paired',
                                     ylab='Positive findings (%)', 
                                     legpos1 = c(0.69, 0.4),
                                     legpos2 = c(0.69, 0.6))

BF_stopH0_power <- plot_grid(BF_stopH0_power_N_CS$power_panel, BF_stopH0_power_N_paired$power_panel,
                           BF_stopH0_power_N_CS$N_panel,BF_stopH0_power_N_paired$N_panel,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 25,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S10.png",
                   plot = BF_stopH0_power,
                   base_width = 40,
                   base_height = 40,
                   units = 'cm')

#################
### SFigure 11 ###
#################
#Power curve BF stop for H1 and H0, Cauchy(1), Paired, Nmax = 30

### Simulate data

#Creates paired one-sided sim-files in "./DerivedData/SimulationData"
for(d in D_vector){
  BF_sim_out <- BF_seq_sim_parallel(paired = T,
                                    n_trials = 30000,
                                    alternative = 'positive',
                                    rscale = 1,
                                    n_comparisons = 100,
                                    ratio_null_true = 0,
                                    mean_diff = d,
                                    standard_dev = 1,
                                    cpus = 4)
  
  saveRDS(object = BF_sim_out,file = paste0('./DerivedData/SimulationData/Paired/OneSided/Supplementary/Cauchy_1/','BFsim_D_',D_vector[d],'.rds'))
  rm(list = c("BF_sim_out")) #clean enviroment of BF_sim_out
}

### Slim data 

#Slim paired one-sided sim-files and save 
files_to_slim <- paste0('./DerivedData/SimulationData/Paired/OneSided/Supplementary/Cauchy_1/BFsim_D_',D_vector,'.rds')
slim_out <- sim_slimmer(files_to_slim = files_to_slim, 
                        n_trials = 15000, 
                        n_comparisons = 100, 
                        paired = T, 
                        alternative = 'positive', 
                        decimals = 1)
saveRDS(object = slim_out,file = paste0('./DerivedData/SlimmedData/Paired/OneSided/Supplementary/Cauchy_1/','BFsims.rds'))
rm(list = c("slim_out")) #clean enviroment of slim_out

### Get plot data 


# cross-sectional one-sided, Nstart = 12, Nmax = 30, Cauchy(1)
pwr.df <-  getPlotData_power_curves(path.slim = './DerivedData/SlimmedData/Paired/OneSided/Supplementary/Cauchy_1/BFsims.rds',
                                    alternative = 'positive',
                                    paired = T,
                                    D_vector = D_vector,
                                    start_n_comparisons = 12,
                                    max_n_comparisons = 30,
                                    test_seq = 1,
                                    k_BF10 = 4,
                                    k_BF01 = 4,
                                    decimals = 1,
                                    NHST_curve = F)

write.csv(x = pwr.df,file = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Cauchy_1/Paired_one_sided_stopH1_stopH0.csv',row.names = F)
rm(list = c("pwr.df")) #clean enviroment of pwr.df


### Plot figure

#paired
BF_stopH0_paired_rscale1 <- plot_Power(path = './DerivedData/PlotData/power_stopH1_stopH0/Supplementary/Cauchy_1/Paired_one_sided_stopH1_stopH0.csv', 
                                       paired = T,
                                       start_n_comparisons = 12,
                                       max_n_comparisons = 30,
                                       D_vector = D_vector,
                                       print_FPR = F, 
                                       title = 'Paired',
                                       ylab='Stop decisions (%)', 
                                       legpos1 = c(0.69, 0.4),
                                       legpos2 = c(0.69, 0.15))


BF_stopH0_paired_rscale1 <- plot_grid(BF_stopH0_paired_rscale1$power_panel,
                             BF_stopH0_paired_rscale1$N_panel,
                           labels = c('A', 'B'), 
                           label_size = 25,ncol = 1)

#And save the plot
cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S11.png",
                   plot = BF_stopH0_paired_rscale1,
                   base_width = 20,
                   base_height = 40,
                   units = 'cm')


##################
### SFigure 12 ###
##################
# Maximum possible BF in favor of H0 for different N when using a Cauchy(0.707). 

source('./R/Figures/getPlotData/Supplementary/getPlotData_maxBF01_at_N.R')
source('./R/Figures/plotFigs/Supplementary/plot_max_BF01_at_N.R')

# Fetch plot data from slimmed Cross-sectional two-sided
max_BF01_at_N.CS.df <- getPlotData_maxBF01_at_N(path.slim = './DerivedData/SlimmedData/CrossSectional/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                             k_BF01 = 4)

write.csv(x = max_BF01_at_N.CS.df,file = './DerivedData/PlotData/Supplementary/maxBF01_at_N/CS_two_sided.csv',row.names = F)

# Fetch plot data from slimmed paired two-sided
max_BF01_at_N.paired.df <- getPlotData_maxBF01_at_N(path.slim = './DerivedData/SlimmedData/Paired/TwoSided/Enlarged_BFsim_D_0/BFsims.rds',
                                             k_BF01 = 4)

write.csv(x = max_BF01_at_N.paired.df,file = './DerivedData/PlotData/Supplementary/maxBF01_at_N/Paired_two_sided.csv',row.names = F)

# Plot figures, Cross-sectional two-sided
max_BF01_at_N.CS <- plot_max_BF01_at_N(path.plotdata = './DerivedData/PlotData/Supplementary/maxBF01_at_N/CS_two_sided.csv',
                                       title = "Cross-sectional")

# Plot figures, Cross-sectional two-sided
max_BF01_at_N.paired <- plot_max_BF01_at_N(path.plotdata = './DerivedData/PlotData/Supplementary/maxBF01_at_N/Paired_two_sided.csv',
                                           title = "Paired")

max_BF01_at_N <- plot_grid(max_BF01_at_N.CS$max_possible_BF01, max_BF01_at_N.paired$max_possible_BF01,
                           max_BF01_at_N.CS$perc_BF01_support_H0,  max_BF01_at_N.paired$perc_BF01_support_H0,
                           labels = c('A', 'B', 'C','D'), 
                           label_size = 10,ncol = 2)

cowplot::save_plot(filename = "./Results/Figures/Supplementary_figures/Figure_S12.png",
                   plot = max_BF01_at_N,
                   base_width = 15,
                   base_height = 15,
                   units = 'cm')
  
