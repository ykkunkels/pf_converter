
#########################################################################
#### A simple function to convert frequency to period and vica versa ####
#### Yoram Kevin Kunkels - 02/08/2018                                ####
####                                                                 ####
#### Operation: Give either Period (p; in min.) or                   ####
####            Frequency (f; in mHz) value to calculate the other.  ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~####

## Function
Period_Frequency_Converter <- function(p = NA, f = NA){
  
  ## Sanity check
  if(is.na(p) & is.na(f)){
    message("Error: Please enter either a Period- or Frequency value!")
    break()
  }
  
  ## Period unknown but Frequency known (in mHz)
  if(is.na(p) & !is.na(f)){

    p <- round((1 / (f * 60) * 1000), 5)
    print(paste("Given Frequency:", f, "mHz"))
    print(paste("Calculated Period: ", round(p), " min ( = ", round((p / 1440), 3), " days )", sep = ""))

  }
  
  ## Frequency unknown but Period known (in min)
  if(!is.na(p) & is.na(f)){
    
    f <- round((((1 / p) / 60) * 1000), 5)
    print(paste("Given Period:", p, "min"))
    print(paste("Calculated Frequency: ", f, " mHz ( = ", round((1 / p), 5), " min^(-1) )", sep = ""))
      
  }
  
}

## Examples
Period_Frequency_Converter(p = (60 * 24)) # 24h period to frequency
Period_Frequency_Converter(f = 0.01157) # 0.01157 mHz frequency to period
 