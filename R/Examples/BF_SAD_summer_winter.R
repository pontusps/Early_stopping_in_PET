###################################################
# Example 2 in article: BF t-test, support for H0 #
###################################################
### Pontus Plavén-Sigray, NRU, August 2020

# BF t-test (H1: d ~ Cauchy(0,sqrt(2)/2) ) applied to [11C]DASB BPND for SAD patients v.s. healthy controls in summer and winter

library(tidyverse)
library(BayesFactor)

data_summer <- read.csv(file = './RawData/McMahon2016/Summer_Winter_HC_SAD_BPND_raw.csv',
                        sep = ',',
                        header = F) %>%
  dplyr::rename(HC_pat = V1, BPND = V2) %>%
  mutate( HC_pat = round(HC_pat)) %>%
  filter( HC_pat < 2) %>% 
  mutate(HC_pat = dplyr::recode(HC_pat, "0" = "HC", "1" = "pat")) 

data_winter <- read.csv(file = './RawData/McMahon2016/Summer_Winter_HC_SAD_BPND_raw.csv',
                        sep = ',',
                        header = F) %>%
  dplyr::rename(HC_pat = V1, BPND = V2) %>%
  mutate( HC_pat = round(HC_pat)) %>%
  filter( HC_pat > 1) %>% 
  mutate(HC_pat = dplyr::recode(HC_pat, "2" = "HC", "3" = "pat")) 

BF10_summer <- BayesFactor::ttestBF(data = data_summer, formula = (BPND ~ HC_pat),mu = 0)
BF10_summer <- as.numeric(as.vector(BF10_summer))[1] 
BF01_summer <- 1/BF10_summer

BF10_winter <- BayesFactor::ttestBF(data = data_winter, formula = (BPND ~ HC_pat),mu = 0)
BF10_winter <- as.numeric(as.vector(BF10_winter))[1] 
BF01_winter <- 1/BF10_winter


