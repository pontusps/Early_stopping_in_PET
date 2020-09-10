#######################################################
# Extract data to plot FPR at different k thresholds 
#######################################################
### Jonas Svensson (KI), July 2020

getplotData_fpr_at_k <- function(path.slim, start_n_comparisons, max_n_comparisons, k_BF10, k_BF01, decimals) {
  
  library(tidyverse)

  source("./R/Figures/source/sub_func_get_slimmed_data.R")
  source("./R/Figures/source/sub_func_fix_rounding.R")
  
  #fetch simulated slimmed data
  sim_data <- sub_func_get_slimmed_data(path.slim = path.slim, decimals, fig_fpr_at_k = 1)

  #Get lengths of loops
  n_trials_per_D <- sim_data$meta_data$ntrials_per_D 
  n_comparisons_per_trial <- sim_data$meta_data$n_comparisons_per_trial 
  n_rows_per_D <- n_trials_per_D * n_comparisons_per_trial
  
  #overwrite list with df
  sim_data <- sim_data$sim_data
  
  save_FPR <- c()
  save_TNR <- c()
  temp_df <- c()
  df_to_plot <- c()
  
  for (k in 1:length(k_BF10)) {
    
    #Run subfunction to correct for rounding error in slimmed files
    sim.data_temp <- sub_func_fix_rounding(k_BF10=k_BF10[k], k_BF01=k_BF01[k], sim_data=sim_data, decimals = decimals)
    
    for (j in start_n_comparisons:max_n_comparisons) {
      
      FPR <- 0
      TPR <- 0
      
      for (i in 0:(n_trials_per_D-1)) {
        
        BF10_temp <- sim.data_temp$BF10[(i*n_comparisons_per_trial+start_n_comparisons):(i*n_comparisons_per_trial + j)]
        BF01_temp <- sim.data_temp$BF01[(i*n_comparisons_per_trial+start_n_comparisons):(i*n_comparisons_per_trial + j)]
        
        if (any(BF10_temp>k_BF10[k])) {
          
          index_BF10 <- min(which(BF10_temp > k_BF10[k]))
          
        }else{
          index_BF10 <- j+1
        }
        
        if (any(BF01_temp>k_BF01[k])) {
          
          index_BF01 <- min(which(BF01_temp > k_BF01[k]))
          
        }else{
          index_BF01 <- j+1
        }
        
        if (index_BF10<j+1|index_BF01<j+1) {
          
          if (index_BF10<index_BF01) {
            FPR <- FPR+1
          }else{
            TPR <- TPR+1
          }
          
        }
        
      }
      
      save_FPR[j-(start_n_comparisons-1)] <- FPR/n_trials_per_D
      save_TNR[j-(start_n_comparisons-1)] <- TPR/n_trials_per_D
      
    }
    
    temp_df <- data.frame(save_FPR)
    temp_df$k <- paste0(as.character(k_BF10[k]))
    temp_df$maxN <- start_n_comparisons:max_n_comparisons
    names(temp_df)[1] <- "FPR"
    
    df_to_plot <- rbind(df_to_plot,temp_df)
    
  }
  
  
  ##Add NHST to the plot-data
  
  NHST <- data.frame(rep(0.05,max_n_comparisons-(start_n_comparisons-1)))
  NHST$k <- "fixed N NHST"
  NHST$maxN <- start_n_comparisons:max_n_comparisons
  names(NHST)[1] <- "FPR"
  
  df_to_plot <- rbind(df_to_plot,NHST)
  
  return(df_to_plot)
  
}
  