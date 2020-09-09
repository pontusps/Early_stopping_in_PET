##############################################################
# Extract data to plot what k keeps FPR < 5% at different Nmax 
##############################################################
### Jonas Svensson (KI), July 2020

### Arguments
#path.slim, path to slimmed data
#start_n_comparisons, at what N the first test is performed. 
#max_n_comparisons, maximum number of subjects to incude before stopping and declare inclusive results.
#start_BF10, starting threshold for BF10
#start_BF01, starting threshold for BF01 (if set to Inf, no stopping will be made for H0)
#fpr_limit, fpr limit 
#step_size, what incremental size BF threshold should be increased by from start_BF, in order to find a threshold corresponding to an fpr = fpr_limit 

getPlotData_k_cntrl_fpr <- function(path.slim, start_n_comparisons, 
                                    max_n_comparisons, start_BF10, start_BF01, 
                                    fpr_limit = 0.05, step_size = 0.05, decimals) {
  
  library(tidyverse)
  library(cowplot)
  
  source("./R/Figures/source/sub_func_get_slimmed_data.R")
  source("./R/Figures/source/sub_func_fix_rounding.R")

  #Run subfunction to load data (D=0)
  sim_data <- sub_func_get_slimmed_data(path.slim = path.slim, decimals, fig_fpr_at_k = 1)
  
  #Get lengths of loops
  n_trials_per_D <- sim_data$meta_data$ntrials_per_D 
  n_comparisons_per_trial <- sim_data$meta_data$n_comparisons_per_trial
  n_rows_per_D <- n_trials_per_D * n_comparisons_per_trial
  
  #overwrite list with df
  sim_data <- sim_data$sim_data
  
  #preallocate
  save_k <- c()
  
  k_BF10_temp <- start_BF10 + step_size
  k_BF01_temp <- start_BF01 + step_size
  
  for (j in start_n_comparisons:max_n_comparisons) {
    
    cat("############# Nmax = ",j,"#################\n")
    
    count <- 0
    
    #to be able to enter while loop
    FPR <- fpr_limit +1
    
    #to allow neutral slope of curve, remove one "step_size"
    
    k_BF10_temp <- k_BF10_temp - step_size
    k_BF01_temp <- k_BF10_temp - step_size
    
    while (FPR > fpr_limit) {
      
      #Increase k threshold to find at which k FPR hits the limit.
      
      if(FPR < fpr_limit+0.01){
        step_size <- 0.01
      }
      
      k_BF10_temp <- k_BF10_temp+step_size
      k_BF01_temp <- k_BF01_temp+step_size
      
      #Run subfunction to correct for rounding in raw files
      sim_data_temp<-sub_func_fix_rounding(k_BF10=k_BF10_temp, k_BF01=k_BF01_temp, sim_data=sim_data, decimals = decimals)
      
      count <- count+1
      FPR <- 0
      
      for (i in 0:(n_trials_per_D-1)) {
        
        BF10_temp <- sim_data_temp$BF10[(i*n_comparisons_per_trial+start_n_comparisons):(i*n_comparisons_per_trial + j)]
        BF01_temp <- sim_data_temp$BF01[(i*n_comparisons_per_trial+start_n_comparisons):(i*n_comparisons_per_trial + j)]
        
        if (any(BF10_temp>k_BF10_temp)) {
          
          index_BF10 <- min(which(BF10_temp > k_BF10_temp))
          
        }else{
          index_BF10 <- j+1
        }
        
        if (any(BF01_temp>k_BF01_temp)) {
          
          index_BF01 <- min(which(BF01_temp > k_BF01_temp))
          
        }else{
          index_BF01 <- j+1
        }
        
        if (index_BF10<j+1|index_BF01<j+1) {
          
          if (index_BF10<index_BF01) {
            FPR <- FPR+1
          }
          
        }
        
        
        
        
      }
      
      FPR <- FPR/n_trials_per_D
      cat(round(FPR,3),"########",k_BF10_temp,"\n")
      
    }
    
    save_k[j-(start_n_comparisons-1)] <- k_BF10_temp
    
    
  }
  
  
  
  df_to_plot <- data.frame(save_k)
  df_to_plot$maxN <- start_n_comparisons:max_n_comparisons
  names(df_to_plot)[1] <- "k"
  
  return(df_to_plot)
  
}
