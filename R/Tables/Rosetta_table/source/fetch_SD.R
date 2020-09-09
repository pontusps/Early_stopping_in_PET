################################################
### Fetch variance from test-retest PET data ###
################################################
## Pontus P. Sigray, August 2019 

fetch_SD <- function(radioligand,paired = F){
  
  trt_data <- xlsx::read.xlsx(file = paste0(file = './RawData/TrTData/Radioliogand_Mean_SDb_SDw.xlsx'),sheetIndex = 1)

  if(paired){
    standard_dev <- dplyr::filter(trt_data,Radioligand == radioligand)$SD_within#within subject SD 
  }else{
    dplyr::filter(trt_data,Radioligand == radioligand)$SD_between  #mean of PET1 and PET2 between subject SD 
  }
  
  return(standard_dev)
}

