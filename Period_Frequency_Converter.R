
#########################################################################
#### A simple function to convert frequency to period and vica versa ####
#### Yoram Kevin Kunkels - 02/08/2018                                ####
####                                                                 ####
#### Operation: Give either Period (p; in min.) or                   ####
####            Frequency (f; in mHz) value to calculate the other.  ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~####

## Function
Period_Frequency_Converter <- function(p = NA, f = NA, f_unit = NA){
  
  ## Sanity checks
  if(is.na(p) & is.na(f)){
    message("Error: Please enter either a Period- or Frequency value!")
    break()
  }
  
  if(!is.na(p) & !is.na(f)){
    message("Error: Please enter either a Period- or Frequency value!")
    break()
  }
  

  ## Period unknown but Frequency known (in mHz)
  if(is.na(p) & !is.na(f)){

    ## Sanity Check
    if(is.na(f_unit)){
      message("Error: Please enter Frequency units (either 'mHz' or 'min^(-1)')!")
      break()
    }
    
    if(f_unit == "mHz"){
         p <- (1 / (f * 60) * 1000)
         print(paste("Given Frequency:", f, "mHz"))
         print(paste("Calculated Period: ", round(p, 2), " min ( = ", round((p / 1440), 3), " days )", sep = ""))
    }
    
    if(f_unit == "min^(-1)"){
      p <- (1 / f)
      print(paste("Given Frequency:", f, "min^(-1)"))
      print(paste("Calculated Period: ", round(p, 2), " min ( = ", round((p / 1440), 3), " days )", sep = ""))
    }
    
  }
  
  ## Frequency unknown but Period known (in min)
  if(!is.na(p) & is.na(f)){
    
    f <- (((1 / p) / 60) * 1000)
    print(paste("Given Period:", p, "min"))
    print(paste("Calculated Frequency: ", round(f, 5), " mHz ( = ", round((1 / p), 5), " min^(-1) )", sep = ""))
      
  }
  
}

## Examples
Period_Frequency_Converter(f = 0.00069, f_unit = "min^(-1)") # 0.01157 mHz frequency to period
Period_Frequency_Converter(p = (24 * 60)) # 24h period to frequency

 