sub_func_fix_rounding <- function(k_BF10, k_BF01, plotData, rounded_to = 0.1) {
  
  
  
  #Subfunction to replace all instances of simulation generated BF values that are exactly same as k_BF10 or k_BF01.
  #Reason for this is to mitigate bias introduced due to rounding (which in turn was done to keep file size low)
  
  
  
  #Create vector with every second element is -0.1 of k_BF10 and every second is +0.1 k_BF10
  lenght_vector_BF10 = length(plotData$BF10[plotData$BF10==k_BF10])
  lenght_vector_BF01 = length(plotData$BF01[plotData$BF01==k_BF01])
  
  x_BF10 =rep(c(k_BF10-rounded_to, k_BF10+rounded_to), times = 2+lenght_vector_BF10)
  x_BF01 =rep(c(k_BF01-rounded_to, k_BF01+rounded_to), times = 2+lenght_vector_BF01)
  
  #Start at random place and use the vector replace values that are == to k_BF10 in big vector
  start_at <- sample(1:2,1)
  
  plotData$BF10<-replace(plotData$BF10,plotData$BF10==k_BF10,x_BF10[start_at:(lenght_vector_BF10+start_at-1)])
  plotData$BF01<-replace(plotData$BF01,plotData$BF01==k_BF01,x_BF01[start_at:(lenght_vector_BF01+start_at-1)])
  
  return(plotData)
  
}