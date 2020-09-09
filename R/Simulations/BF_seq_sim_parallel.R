#######################################################################################
### Early stopping in PET studies using Bayes Factor: Code for parallel processing  ###
#######################################################################################
### Pontus P. Sigray, August 2019

###Arguments
#n_trials, number of simulated trials 
#n_comparisons, number of patient-control pairs (if paired = F) or number of pre-post scans (if paired = T).  
#mean_diff, a number indicating the true population mean difference 
#sd, a number indicating the standard deviation within or between subjects (default set to 1, making  'mean_diff' into Cohen's D or Cohen's Dz).  
#rscale, number specifiying the r-scale ("standard deviation") of the Cauchy distribution that is representing the alternative hypothesis (H1).   
#alternative, a character string specifying the alternative hypothesis, must be one of "two.sided" or "positive" or "negative". 
#paired, a logical indicating whether you want a paired t-test.
#ratio_null_true, a number between 0 to 1 specifiying for how large propotion of trials the H0 is true. If mean_diff = 0, ratio_null_true is always 1.   
#use_seeds, optional vector of saved random seeds to reproduce a previous simulation. Must be the same length as 'n_trials'.  
#cpus, number of cpus to use for parallel processing (default cpus = 1, meaning serial processing)


###Fun for parallel processing on multiple cores for sequential testing of BF
simFun <- function(i,n_comparisons,sim,paired,alternative,rscale){
  
    #Preallocate variables to be saved within each trial
    n_comparisons_trial_saved <- vector(length = n_comparisons-1) 
    BF_10_trial_saved <- vector(length = n_comparisons-1) 
    BF_01_trial_saved <- vector(length = n_comparisons-1)  
    p_val_trial_saved <- vector(length = n_comparisons-1)  
    Trial_trial_saved <- vector(length = n_comparisons-1)  
    mean_trial_saved <- vector(length = n_comparisons-1)  
    sd_trial_saved <- vector(length = n_comparisons-1)  
    
    #Perform BF t-test, starting at n_comparisons = 2, and add one comparison until max comparisons is reached. 
    for (s in 2:n_comparisons) {
      
      ### Paired t-test (Longitudinal design)
      if (paired == T){
        #Calculate BF in favor of H1
        if (alternative == 'two.sided'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$deltaPairs[1:s], rscale = rscale,mu = 0) #a one-sample t-test on the paired difference against 0, equivalent to a paired sample t-test
          #p_val <- t.test(x = sim[[i]]$data$deltaPairs[1:s])$p.value
        }else if(alternative == 'negative'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$deltaPairs[1:s], rscale = rscale, nullInterval = c(-Inf,0), mu = 0) #a one-sample t-test on the paired difference against 0, equivalent to a paired sample t-test
          #p_val <- t.test(x = sim[[i]]$data$deltaPairs[1:s],alternative = 'less')$p.value
        }else if(alternative == 'positive'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$deltaPairs[1:s], rscale = rscale, nullInterval = c(0,Inf), mu = 0)  #a one-sample t-test on the paired difference against 0, equivalent to a paired sample t-test
          #p_val <- t.test(x = sim[[i]]$data$deltaPairs[1:s],alternative = 'greater')$p.value
        }
        
        mean_trial_saved[s-1] <- mean(sim[[i]]$data$deltaPairs[1:s])
        sd_trial_saved[s-1] <- sd(sim[[i]]$data$deltaPairs[1:s])
        
        ### Two-sample t-test (Cross-sectional design)
      } else if (paired == F) {
        #Calculate BF in favor of H1
        if (alternative == 'two.sided'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s], rscale = rscale)
          #p_val <- t.test(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s])$p.value
        }else if(alternative == 'negative'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s], rscale = rscale, nullInterval = c(-Inf,0))
          #p_val <- t.test(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s],alternative = 'less')$p.value
        }else if(alternative == 'positive'){
          BF_10 <- BayesFactor::ttestBF(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s], rscale = rscale, nullInterval = c(0,Inf))
          #p_val <- t.test(x = sim[[i]]$data$group1[1:s],y = sim[[i]]$data$group2[1:s],alternative = 'greater')$p.value
        }
        
        mean_trial_saved[s-1] <- mean(sim[[i]]$data$group1[1:s]) - mean(sim[[i]]$data$group2[1:s])
        sd_trial_saved[s-1] <-   mean(c(sd(sim[[i]]$data$group1[1:s]),sd(sim[[i]]$data$group2[1:s])))
      }
        
      
      BF_10 <- as.numeric(as.vector(BF_10))[1] 
      
      #Calculate BF in favor of H0
      BF_01 <- 1/BF_10 
      
      #Save results
      n_comparisons_trial_saved[s-1] <- s
      BF_10_trial_saved[s-1] <- BF_10 
      BF_01_trial_saved[s-1] <- BF_01 
      #p_val_trial_saved[s-1] <- p_val
      
      Trial_trial_saved[s-1] <- i
      
    } #end of "for (s in 2:n_comparisons) {" loop 

      
      return(list(mean_trial_saved = unlist(mean_trial_saved), 
                  sd_trial_saved = unlist(sd_trial_saved),
                  n_comparisons_trial_saved = unlist(n_comparisons_trial_saved),
                  BF_10_trial_saved = unlist(BF_10_trial_saved), 
                  BF_01_trial_saved = unlist(BF_01_trial_saved), 
                  #p_val_trial_saved = unlist(p_val_trial_saved), 
                  Trial_trial_saved = unlist(Trial_trial_saved)) )
      
}

###Wrapper fun to execute simFun above and save and write results 
BF_seq_sim_parallel <- function(n_trials = 100, n_comparisons = 100, mean_diff = 0, standard_dev = 1, rscale = sqrt(2)/2, alternative = 'two.sided', paired = F, ratio_null_true = 0, use_seeds = F, cpus = 1){
  
  source('./R/Simulations/simulate_data.R')
  
  #Start time 
  t0 <- Sys.time()
  
  #Preallocate variables to be saved on trial level
  n_comparisons_saved <- NULL
  BF_10_saved <- NULL
  BF_01_saved <- NULL
  #pval_saved <- NULL
  Trial_saved <- NULL
  mean_diff_at_n_saved <- NULL
  sd_at_n_saved <- NULL
  
  mean_diff_data <- vector(length = n_trials)
  sd_data <- vector(length = n_trials)
  trial_number <- vector(length = n_trials)
  Htrue <- vector(length = n_trials)
  saved_seeds <- vector(length = n_trials)
 
  ##################
  ### Simulation ### 
  ##################
  
  #Load saved seed or set and save random seed
  # if (use_seeds == F) {
  #   seed <- sample(x = -1e9:1e9,1)
  #   saved_seeds[i] <- seed
  #   set.seed(seed)
  # }else{
  #   saved_seeds[i] <- set.seed(use_seeds[i])
  #   set.seed(use_seeds[i])
  # }
  
  #Simulate data for all trials
  sim <- simulate_data(n_trials = n_trials, 
                       n_comparisons = n_comparisons, 
                       mean_diff = mean_diff, 
                       standard_dev = standard_dev, 
                       paired = paired,
                       ratio_null_true = ratio_null_true)
  
  ########################################################################
  # Parallel processing - run BF tests in parallel on 4 cores within each trial
  ########################################################################
  #Code is a modified version from: 
  
  # initiate a parallel cluster with 4 cpus
  library(snowfall)
  snowfall::sfInit(parallel = T, cpus = cpus)
  
  #Export simFun and all it's input arguments to the clusters
  snowfall::sfExport("simFun","n_comparisons","sim","paired","alternative","rscale")
  
  ### Execute the parallel code
  #Perform BF t-test, starting at n_comparisons = 2, and add one comparison until max comparisons is reached. See fun simFun above. 
  BF_sim_out <- snowfall::sfClusterApplyLB(1:n_trials, function(i) simFun(i, n_comparisons, sim, paired, alternative, rscale))
  
  #Stop the clusters
  snowfall::sfStop()
  
  #Save remaining data on trial level
  
  for (i in 1:n_trials){
    n_comparisons_saved <- c(n_comparisons_saved, c(1,BF_sim_out[[i]]$n_comparisons_trial_saved)) #Add first comparison as first BF (at n_comparisons = 1) was set to 1 
    BF_10_saved <- c(BF_10_saved,c(1,BF_sim_out[[i]]$BF_10_trial_saved)) #save first comparison (n_comparisons = 1) as BF = 1 as this is not tested
    BF_01_saved <- c(BF_01_saved,c(1,BF_sim_out[[i]]$BF_01_trial_saved)) #save first comparison (n_comparisons = 1) as BF = 1 as this is not tested
    #pval_saved <- c(pval_saved,c(NA,BF_sim_out[[i]]$p_val_trial_saved)) #save first comparison (n_comparisons = 1) as p = NA as this is not tested
    Trial_saved <- c(Trial_saved,c(BF_sim_out[[i]]$Trial_trial_saved[1],BF_sim_out[[i]]$Trial_trial_saved)) #Add trial index to position 1 as first BF (at n_comparisons = 1) was set to 1 
    mean_diff_at_n_saved <- c(mean_diff_at_n_saved,c(NA,BF_sim_out[[i]]$mean_trial_saved))
    sd_at_n_saved <-  c(sd_at_n_saved,c(NA,BF_sim_out[[i]]$sd_trial_saved))
    
    trial_number[i] <- i
    Htrue[i] <-  sim[[i]]$Htrue
    
    if (paired == T){
      mean_diff_data[i] <- mean(sim[[i]]$data$deltaPairs)
      sd_data[i] <- sd(sim[[i]]$data$deltaPairs)
    } else if (paired == F){
      mean_diff_data[i] <- mean(sim[[i]]$data$group1 - sim[[i]]$data$group2 )
      sd_data[i] <- mean(c( sd(sim[[i]]$data$group1), sd(sim[[i]]$data$group2) ))
    }
  }
  
  ##############
  ### Return ###
  ##############
  
  BFs <- data.frame(Trial = Trial_saved, 
                    n_comparisons = n_comparisons_saved, 
                    BF_10 = BF_10_saved, 
                    BF_01 = BF_01_saved, 
                    #pval = pval_saved,
                    mean_diff_at_n_saved = mean_diff_at_n_saved,
                    sd_at_n_saved = sd_at_n_saved) 
  
  Trial_info <- data.frame(trial_number = trial_number, 
                           Htrue = Htrue, 
                           mean_diff_data = mean_diff_data, 
                           sd_data = sd_data, 
                           saved_seeds = saved_seeds )
  
  Sim_info <- list(n_trials = n_trials,
                   n_comparisons = n_comparisons,
                   effectsize_sim = mean_diff/standard_dev,
                   mean_diff_sim = mean_diff,
                   sd_sim = standard_dev, 
                   rscale = rscale,
                   alternative = alternative,
                   paired = paired,
                   ratio_null_true = ratio_null_true,
                   used_saved_seeds = use_seeds,
                   time_sec = Sys.time() - t0 ) 
  
  out <- list(Trial_info = Trial_info,BFs =  BFs, Sim_info = Sim_info)
  
  return(out)

}

