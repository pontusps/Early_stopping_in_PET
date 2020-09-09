########################################################################################################
### Convert Cohen's D or DZ to raw difference or vice versa using SD from test-retest PET/SPECT data ###
########################################################################################################
## Pontus P. Sigray, September 2019 

#measures: vector of values to be converted. Eg. c(0.2, 0.5, 0.8) 
#type_in: char denoting what kind of measure is inputted  'Dz' or 'D' or 'raw'. 
#type_out: char denoting what kind of measure is outputted. 'Dz' or 'D' or 'raw'. If 'type_in' is 'D' or 'Dz', 'type_out' must be 'raw'
#radioligand: what radioligand (TrT study) to base the conversion on. E.g. 'Raclopride_HRRT'   

convert_effsize_rawdata <- function(measures, type_in = 'D', type_out = 'raw', radioligand){
  
  trt_data <- xlsx::read.xlsx(file = './RawData/TrTData/Radioliogand_Mean_SDb_SDw.xlsx',sheetIndex = 1)
  
  standard_dev_b <- trt_data$SD_between[trt_data$Radioligand == radioligand]  #mean of PET1 and PET2 between subject SD 
  standard_dev_w <- trt_data$SD_within[trt_data$Radioligand == radioligand] #within subject SD 
  
  if(type_in == 'D' & type_out == 'Dz'){
    
    raw_diff = measures*standard_dev_b 
    out = raw_diff/standard_dev_w
    
  }else if(type_in == 'D' & type_out == 'raw') {

    out = measures*standard_dev_b
    
  }else if(type_in == 'Dz' & type_out == 'D') { 
    
    raw_diff = measures*standard_dev_w 
    out = raw_diff/standard_dev_b
    
  }else if(type_in == 'Dz' & type_out == 'raw'){
    
    out = measures*standard_dev_w 
    
  }else if(type_in == 'raw' & type_out == 'D'){
    
    out = measures/standard_dev_b
    
  }else if(type_in == 'raw' & type_out == 'Dz'){
  
    out = measures/standard_dev_w
    
  }
  
  return(out)
}

