### Fetch BF threshold (k) for keeping fpr below 5% for different Nmax
#PPS, NRU, October 2020

sub_func_fetch_k <- function(path.data, N_max_compairsons){

  library(tidyverse)
  k_data <- read.csv(path.data)  
  
  k <- k_data %>% filter(maxN == N_max_compairsons)
  return(k$k)
}
