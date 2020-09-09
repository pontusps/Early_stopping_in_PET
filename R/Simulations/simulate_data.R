#################################################
### Simulated data for running seq BF testing ###
#################################################
## Pontus P. Sigray, August 2019

#Script returns simulated differences in a longitudinal (paired = T) or cross-sectional (paired = F) design. 

simulate_data <- function(n_trials = 1, n_comparisons = 20, mean_diff = 0, standard_dev = 1, paired = F, ratio_null_true = 0){
  
  #Preallocate object for storing simulated data
  out <- NULL
  
  for (t in 1:n_trials){
    
    #Preallocate
    data <- list()
    
    #Simulate data with H0 or H1 being true
    null_true <-  ifelse( sample(x = 1:100,size = 1)/100 <= ratio_null_true, 1, 0)
    
    #simulate data with H0 true
    if(null_true == 1){
      
      if(paired==T){
        data$deltaPairs <- rnorm(n = n_comparisons,mean = 0,sd = standard_dev) 
      }else if(paired==F){
        data$group1 <- rnorm(n = n_comparisons,mean = mean_diff,sd = standard_dev) 
        data$group2 <- rnorm(n = n_comparisons,mean = mean_diff,sd = standard_dev) 
      }
      
      Htrue <- 'H0' #save which hypothesis is true
    
    #simulate data with H1 true   
    }else{

      if(paired==T){
        data$deltaPairs <- rnorm(n = n_comparisons,mean = mean_diff,sd = standard_dev) #simulate data with H0 true
        
      }else if(paired==F){
        data$group1 <- rnorm(n = n_comparisons,mean = 0 + mean_diff,sd = standard_dev) #if mean_diff is positive, then the group difference (group1 - group2) becomes positive and vice versa  
        data$group2 <- rnorm(n = n_comparisons,mean = 0,sd = standard_dev) 
        
      }
      
      Htrue <- 'H1' #save which hypothesis is true
    }
    
  out[[t]] <- list(data = data,Htrue = Htrue, SDpopulation =  standard_dev)
    
    
  }
  #Return simulated data and which hypothesis is true
  return(out)

}
