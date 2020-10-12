############################################
# "Power curves" for sequential BF testing #
############################################
# Jonas Svensson and Pontus Plavén-Sigray, KI, August 2020

### Arguments
#path.slim, path to slimmed data
#alternative, either two.sided or "positive" or "negative" 
#paired, either FALSE for cross-sectional test or TRUE for paired test
#start_n_comparisons, at what N the first test is performed. 
#max_n_comparisons, maximum number of subjects to incude before stopping and declare inclusive results.
#test_seq, how often perform tests. If set to 1, a test will be performed after each added comparison-pair; 2 every second etc. Last subject (max_n_comparisons) always tested. 
#k_BF10, at what BF to stop for H1
#k_BF01, at what BF to stop for H0. If set to Inf, stopping for H0 becomes impossible.
#decimals, specify decimal precision of slimmed data
#NHST_curve, if set to T, adds a power curve for NHST (t.test) using N=max_n_comparisons
#sig.level, significance level for NHST power curve (NHST_curve)

getPlotData_power_curves <- function(path.slim, alternative, paired, D_vector, start_n_comparisons,
                                     max_n_comparisons, test_seq = 1, k_BF10, k_BF01, decimals, NHST_curve, 
                                     sig.level = 0.05){
  
  library(tidyverse)
  library(cowplot)
  library(pwr)
  
  source("./R/Figures/source/sub_func_get_slimmed_data.R")
  source("./R/Figures/source/sub_func_fix_rounding.R")
  source("./R/Figures/source/sub_func_errortest.R")
  source("./R/Figures/source/sub_func_NHST.R")
  
  #Run subfunction to test for errors in input:
  sub_func_errortest(start_n_comparisons=start_n_comparisons, 
                     max_n_comparisons = max_n_comparisons, 
                     test_seq=test_seq, k_BF01=k_BF01, k_BF10=k_BF10)
  
  
  #Run subfunction to load data
  sim_data <- sub_func_get_slimmed_data(path.slim = path.slim, decimals, fig_fpr_at_k = NULL)
  
  #Get lengths of loops
  n_trials_per_D <- sim_data$meta_data$ntrials_per_D 
  n_comparisons_per_trial <- sim_data$meta_data$n_comparisons_per_trial 
  n_rows_per_D <- n_trials_per_D * n_comparisons_per_trial
  
  #Overwrite list with df
  sim_data <- sim_data$sim_data
  
  #Run subfinction to correct for rounding in raw files
  sim_data <- sub_func_fix_rounding(k_BF10=k_BF10, k_BF01=k_BF01, sim_data=sim_data, decimals = decimals)
  
  #Round k to one decimale (if user supplies more than one decimale a warning will be shown)
  k_BF10 <- round(k_BF10,1)
  k_BF01 <- round(k_BF01,1)
  
  #create vector which tells function at what N to test, and another that tells what N this corresponds to in 
  #each simulated study
  last_test <- (max_n_comparisons-start_n_comparisons)+1
  
  test_at <- seq(1,last_test,test_seq)
  at_N <- seq(start_n_comparisons,max_n_comparisons,test_seq)
  
  #add max_n_comparisons to be tested if this was not done initially
  if (test_at[length(test_at)]!= last_test) {
    
    test_at <- c(test_at, last_test)
    at_N <- c(at_N,max_n_comparisons)
  }
  
  #DFs for saving in loop
  save_BF10 <- c()
  save_BF01 <- c()
  stopped_at <- c()
  mean_stopped_at <- c()
  SD_N <- c()
  
  for (j in 0:(length(D_vector)-1)) {
    
    
    #Extract simulation data for each D, probably pointless and might slow stuff down
    temp_BF10 <- sim_data$BF10[(1+j*n_rows_per_D):(j*n_rows_per_D+n_rows_per_D)]
    temp_BF01 <- sim_data$BF01[(1+j*n_rows_per_D):(j*n_rows_per_D+n_rows_per_D)]
    
    #Reset temp variables
    BF10_pos <- 0
    BF01_pos <- 0
    stopped_at <- 0
    
    for (i in 1:n_trials_per_D) {
      
      #Rip out vector of BF01 and BF10 that is to be checked 
      BF10_temp <- temp_BF10[((i-1)*n_comparisons_per_trial+start_n_comparisons):(((i)*n_comparisons_per_trial)-(n_comparisons_per_trial-max_n_comparisons))]
      BF01_temp <- temp_BF01[((i-1)*n_comparisons_per_trial+start_n_comparisons):(((i)*n_comparisons_per_trial)-(n_comparisons_per_trial-max_n_comparisons))]
      
      #If any BF10 is above k, find the index in the vector (need to add Nstart to get the actual N where we stopped) 
      if (any(BF10_temp[test_at]>=k_BF10)) {
        
        index_BF10 <- min(which(BF10_temp[test_at] >= k_BF10))
        
      }else{
        index_BF10 <- max_n_comparisons+1 #Else set index to a number we cannot have reached
      }
      #Same procedure for BF01
      if (any(BF01_temp[test_at]>=k_BF01)) {
        
        index_BF01 <- min(which(BF01_temp[test_at] >= k_BF01))
        
      }else{
        index_BF01 <- max_n_comparisons+1
      }
      #If either BF10 or BF01 have stopped at or before max_n_comparisons we find out where and save that N, else we call it inconclusive and set N to max_n_comparisons
      if (index_BF10<max_n_comparisons+1|index_BF01<max_n_comparisons+1) {
        
        if (index_BF10<index_BF01) {
          BF10_pos <- BF10_pos+1
          stopped_at[i] <- at_N[index_BF10]
        }else{
          BF01_pos <- BF01_pos+1
          stopped_at[i] <- at_N[index_BF01]
        }
        
      }else{
        stopped_at[i] <- max_n_comparisons 
      }
      
    }
    
    #Save the interesting stuff for each D
    save_BF10[j+1] <- BF10_pos/n_trials_per_D
    save_BF01[j+1] <- BF01_pos/n_trials_per_D
    mean_stopped_at[j+1]<- mean(stopped_at)
    SD_N[j+1] <- sd(stopped_at)
    
  }
  
  #Create df to plot
  BF_10_pos <- data.frame(Decision = save_BF10*100,  
                          N = mean_stopped_at, #make decision into %
                          SD_ymax = mean_stopped_at + SD_N,
                          SD_ymin = mean_stopped_at - SD_N) 
  BF_10_pos$outcome <- "Stop for H1"
  BF_10_pos$D <- D_vector

  BF_pos <- data.frame(Decision = (save_BF01+save_BF10) *100,  
                       N = mean_stopped_at, #make decision into %
                       SD_ymax = mean_stopped_at + SD_N,
                       SD_ymin = mean_stopped_at - SD_N) 
  
  BF_pos$outcome <- "Stop for H1 OR H0"
  BF_pos$D <- D_vector

  BF_01_pos <- data.frame(Decision = save_BF01 *100,  N = mean_stopped_at, #make decision into %
                          SD_ymax = mean_stopped_at + SD_N,
                          SD_ymin = mean_stopped_at - SD_N) 
  
  BF_01_pos$outcome <- "Stop for H0"
  BF_01_pos$D <- D_vector

  if (NHST_curve==T){
    NHST <- sub_func_NHST(alternative=alternative,paired=paired, max_n_comparisons=max_n_comparisons, D_vector=D_vector, sig.level)
    
    NHST <- data.frame(Decision = NHST*100, 
                       N = max_n_comparisons, 
                       SD_ymax = mean_stopped_at + SD_N,
                       SD_ymin = mean_stopped_at - SD_N )
    NHST$outcome <- "fixed N approach"
    NHST$D <- D_vector

    
    if (k_BF01 == Inf){
      BF_10_pos$outcome <- "BF seq testing"
      df_to_plot <- rbind(BF_10_pos,NHST)

    }else{
      df_to_plot <- rbind(BF_10_pos,BF_01_pos, NHST)
    }
    
  }else{
    df_to_plot <- rbind(BF_10_pos,BF_01_pos, BF_pos)  
  }
  
  return(df_to_plot)
  
}
  