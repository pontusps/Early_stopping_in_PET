###########################################################################
# Get data for plotting the max possible BF in favor of H0 at different N #
###########################################################################

### Arguments
# path, path to slimmed data
# k_BF01, threshold for declaring support in favor of H0

getPlotData_maxBF01_at_N <- function(path.slim,k_BF01){
  
  library(tidyverse)
  
  BF_threshold <- k_BF01

  #Read sim-data and create plot data 
  simres <- readRDS(file = path.slim)
  BF_sims <- simres$slimmed.data
  BF_sims$BF.vec.int <- BF_sims$BF.vec.int / BF_sims$BF.vec.int[1] #retrieve correct number of decimals
  
  #Get BF_10 and BF_01
  BF_sims$BF_10 <- ifelse(test = (BF_sims$BF10.true),yes = BF_sims$BF.vec.int, no = 1/BF_sims$BF.vec.int)
  BF_sims$BF_01 <- 1/BF_sims$BF_10
  
  tot_trials <- simres$meta.data$ntrials_per_D * length(simres$meta.data$slimmed_files)
  max_n_comparisons <- simres$meta.data$n_comparisons_per_trial
  
  BF_sims <- BF_sims %>%
    mutate(n_comparisons = rep(1:max_n_comparisons,tot_trials) ) 
  
  BF_sims <- BF_sims %>%
    select( c(n_comparisons,BF_10, BF_01) )
  
  #Summarise the results
  maxBF01_at_N <- BF_sims %>%
    dplyr::group_by(n_comparisons) %>%
    dplyr::summarise(maxBF01_at_N = max(BF_01),
                     perc_BF01_above_BF_threshold = 100*(sum(BF_01 > BF_threshold)/length(BF_01)))
  
  return(maxBF01_at_N)

}