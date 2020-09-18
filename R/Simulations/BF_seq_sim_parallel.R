###############################################################################################################################
### Maximum BF for Bayesian t-test (BayesFactor::ttestBF) using point H0 and a zero centered Cauchy dist for prior over H1  ###
###############################################################################################################################
## Jonas Svensson and Pontus P. Sigray, Karolinska Institutet and NRU Rigshospitalet, Jan 2020

# Parallelization code modified from https://datashenanigan.wordpress.com/2016/01/15/speeding-bayesian-power-analysis-t-test-up-with-snowfall/ 

#Install the following packages
#install.packages(BayesFactor)
#install.packages(snowfall)
#install.packages(dplyr)
#install.packages(purrr)
#install.packages(magrittr) 
#BF_threshold: threshold for Bayes Factor  
#D: simulated difference in means
#sd: SD of simulated data ("SD within" if paired=T, "SD between" if paired = F) 
#ntrials: number of simulations
#N_start: N subjects to start calculating BF at (minimum value is 3)
#N_stop: N subjects to stop calculating BF at
#rscale: scale of the Cauchy distribution
#paired: if TRUE t-test will be one-sample, if F t-test will be two-sample
#greater: if TRUE run one-sided t-test expecting effect to be positive (H1: d ~ Cauchy(0, rscale)T[0,Inf]). if FALSE a two-sided t-test will be run. 
#cpus: number of cpu to run parallelization on. Defualt = 1 which is the same as serial processing. 

######################################################################################################################
# Wrapper for calculating and presenting maximum BF10 and BF01 for data simulated with a mean difference of D and sd # 

maxBF_at_N <- function(BF_threshold = 3, D=0, sd = 1,ntrials = 100, N_start = 3, N_stop = 40, rscale = sqrt(2)/2, paired = T, greater = F, cpus = 1){
  
  #load package for doing pipes
  library(magrittr)
  
  #load package for running parallel (wrappers for 'snow')
  library(snowfall)
  
  ### Define function to run simulations in parallell using the "snowfall" package 
  simFun <- function(t, D, sd, N_start, N_stop, rscale, paired, greater){
    
    #Define vars to be saved
    BF10_all <- NULL
    BF01_all <- NULL
    Trial_nbr <- NULL
    N <- NULL
    
    #two-sided or one-sided: define lower bound of ttestBF( nullInterval )  
    lower_b <- ifelse(greater==F,-Inf,0)
    
    ### Simulate data and calculate BF for two_sample or paired/one-sample design
    
    #for paired/one-sample t-test
    if (paired == T){
      
      #simulate data
      sim.data <- rnorm(n = N_stop, mean = D,sd = sd)
      
      #Calc BF10 at N_start subjects, and then at N_start+1 subjects until N_stop subjects are reached
      BF10_trial <- sapply(N_start:N_stop, function(x) as.numeric(as.vector(BayesFactor::ttestBF(x = sim.data[1:x], rscale = rscale, nullInterval = c(lower_b,Inf) )))  )
      
      # for two-sample t-test  
    } else if (paired == F){
      
      #simulate data
      g1 <- rnorm(n = N_stop, mean = 0,sd = sd)
      g2 <- rnorm(n = N_stop, mean = D,sd = sd)
      
      sim.data <- data.frame(g1 = g1, g2 = g2)
      
      #Calc BF10 at N_start subjects, and then at N_start+1 subjects until N_stop subjects are reached
      BF10_trial <- sapply(N_start:N_stop, function(x) as.numeric(as.vector(BayesFactor::ttestBF(sim.data$g1[1:x], sim.data$g2[1:x], nullInterval = c(lower_b,Inf) )))  )
      
    }
    
    #Calc BF01 for N_start subjects, and then N_start+1 subjects, until N_stop subjects are reached
    BF01_trial <- 1/BF10_trial
    
    #Save vars
    BF10_all <- c(BF10_all,BF10_trial)
    BF01_all <- c(BF01_all,BF01_trial)
    N <- c(N,N_start:N_stop)
    Trial_nbr <- c(Trial_nbr, rep(t, length(N_start:N_stop)))
    
    return( list(BF10_all = unlist(BF10_all), BF01_all = unlist(BF01_all),N = unlist(N), Trial_nbr = unlist(Trial_nbr)) )
  }
  
  ### Define function to execute simulation script above on cluster and tidy up the output 
  maxBF_at_N_sim <- function(D=0, sd = 1,ntrials = 100, N_start = 3, N_stop = 140, rscale = sqrt(2)/2, paired = F, greater = F, cpus = 1){
    
    # initiate a parallel cluster with x number of cpus
    snowfall::sfInit(parallel = T, cpus = cpus)
    
    # export the function to the clusters
    snowfall::sfExport("simFun", "D","sd","N_start","N_stop","rscale","paired","greater")
    
    # execute the code
    BF_sims <- snowfall::sfClusterApplyLB(1:ntrials, function(t) simFun(t,D = 0, sd = 1, N_start = N_start,N_stop = N_stop,rscale = rscale, paired = paired, greater = greater))
    
    # Stop the clusters
    snowfall::sfStop()
    
    #Create df out of sim output
    BF_out <- data.frame(Trial_nbr =   unlist(purrr::map(BF_sims,"Trial_nbr")), 
                         N =  unlist(purrr::map(BF_sims,"N")),
                         BF10 =  unlist(purrr::map(BF_sims,"BF10_all")),
                         BF01 =  unlist(purrr::map(BF_sims,"BF01_all")))
    
    return(BF_out)
    
  }
  
  #Retrieve simulated tidy data 
  BF_out <- maxBF_at_N_sim(D, sd, ntrials, N_start, N_stop, rscale, paired, greater, cpus)
  
  #Summarise the results
  maxBF01_at_N <- BF_out %>%
    dplyr::group_by(N) %>%
    dplyr::summarise(maxBF01_at_N = max(BF01))
  
  maxBF10_at_N <- BF_out %>%
    dplyr::group_by(N) %>%
    dplyr::summarise(maxBF10_at_N = max(BF10))
  
  perc_BF01_above_BF_threshold_at_N <- BF_out %>%
    dplyr::group_by(N) %>%
    dplyr::summarise( perc_BF01_above_BF_threshold = 100*(sum(BF01 > BF_threshold)/length(BF01)) )
  
  #Create plots showing number of trials crossing over BF_threshold at each N
  
  if (paired == F){
    sample <- "two sample"
  }else{
    sample <- "one sample"
  }
  
  if (greater == F){
    sided <- "two sided"
  }else{
    sided <- "one sided"
  }
  
  title_BF01_BF_threshold <- paste0("Percent trials with support (BF01>",BF_threshold,") for true H0: d=",D," at increasing N, \n ",sample," , ",sided," test, H1: d~Cauchy (0,", round(rscale,3),")")
  
  plot(perc_BF01_above_BF_threshold_at_N$N,perc_BF01_above_BF_threshold_at_N$perc_BF01_above_BF_threshold, 
       axes=FALSE, 
       xlim=c(0,N_stop),
       ylim=c(0,100), 
       xlab = 'N subjects/group',
       ylab = paste0('% BF01 > ',BF_threshold))+
    axis(side = 1, at=seq(0,N_stop,3)) +
    axis(side = 2, at=seq(0,100,10)) +
    title(main = title_BF01_BF_threshold)
  
  perc_BF01_above_BF_threshold_at_N <- recordPlot()
  
  #Make plot showing max possible BF01 for each N
  title_max_BF01 <- paste0("Max possible Bayes Factor in favor of H0: d=",D," at increasing N, \n ",sample," , ",sided," test, H1: d~Cauchy (0,", round(rscale,3),")")
  plot(maxBF01_at_N$N,maxBF01_at_N$maxBF01_at_N, 
       axes=FALSE, xlim=c(0,N_stop), 
       ylim=c(0,max(maxBF01_at_N$maxBF01_at_N)+1),
       ylab = 'Max BF01', xlab = 'N subjects/group') +
    axis(side = 1, at=seq(0,N_stop,3)) +
    axis(side = 2, at=0:(max(maxBF01_at_N$maxBF01_at_N)+2)) +
    title(main = title_max_BF01)
  
  maxBF01_at_N_plot <- recordPlot()
  
  #Make plot showing max possible BF10 for each N
  title_max_BF10 <- paste0("Max possible Bayes Factor in favor of H1: d~cauchy (0,", round(rscale,3),") \n H0: d=0, ",sample,", ",sided," t-test, \n Mean diff = ",D, " SD = ", sd)
  plot(maxBF10_at_N$N,maxBF10_at_N$maxBF10_at_N, 
       axes=FALSE, xlim=c(0,N_stop), 
       ylim=c(0,max(maxBF10_at_N$maxBF10_at_N)+1),
       ylab = 'Max BF10', xlab = 'N subjects/group') +
    axis(side = 1, at=seq(0,N_stop,3)) +
    axis(side = 2, at=0:(max(maxBF10_at_N$maxBF10_at_N)+2)) +
    title(main = title_max_BF10)
  
  maxBF10_at_N_plot <- recordPlot()
  
  #Return dataframe and plots 
  return(list(BF_simulations = BF_out, maxBF01_at_N_plot = maxBF01_at_N_plot, maxBF10_at_N_plot = maxBF10_at_N_plot, perc_BF01_above_BF_threshold_at_N = perc_BF01_above_BF_threshold_at_N ))
  
}

### end of script 