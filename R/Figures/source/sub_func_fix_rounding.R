sub_func_fix_rounding <- function(k_BF10, k_BF01, sim_data, decimals) {
  
  chg <- 1/10^decimals
  
  #Subfunction to replace all instances of simulation generated BF values that are exactly same as k_BF10 or k_BF01.
  #Reason for this is to mitigate bias introduced due to rounding (which in turn was done to keep file size low)
  
  #Create vector with every second element is -0.1 of k_BF10 and every second is +0.1 k_BF10
  lenght_vector_BF10 = length(sim_data$BF10[sim_data$BF10==k_BF10])
  lenght_vector_BF01 = length(sim_data$BF01[sim_data$BF01==k_BF01])
  
  x_BF10 =rep(c(k_BF10-chg, k_BF10+chg), times = 2+lenght_vector_BF10)
  x_BF01 =rep(c(k_BF01-chg, k_BF01+chg), times = 2+lenght_vector_BF01)
  
  #Start at random place and use the vector replace values that are == to k_BF10 in big vector
  start_at <- sample(1:2,1)
  
  sim_data_temp <- NULL
  sim_data_temp$BF10<-replace(sim_data$BF10,sim_data$BF10==k_BF10,x_BF10[start_at:(lenght_vector_BF10+start_at-1)])
  sim_data_temp$BF01<-replace(sim_data$BF01,sim_data$BF01==k_BF01,x_BF01[start_at:(lenght_vector_BF01+start_at-1)])
  
  return(sim_data_temp)
  
}