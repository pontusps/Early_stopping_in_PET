############################################
### Fetch mean from test-retest PET data ###
############################################
## Pontus P. Sigray, Nov 2019 

fetch_mean <- function(radioligand){
  
  trt_data <- xlsx::read.xlsx(file = paste0(file = './RawData/TrTData/Radioliogand_Mean_SDb_SDw.xlsx'),sheetIndex = 1)
  
  mean_val <- dplyr::filter(trt_data,Radioligand == radioligand)$mean
  
  return(mean_val)
}

