f_compute_power_spectra <- function(PwData, sliders) {
  
  Power_Spectra = data.frame()  
  for (id in unique(PwData$subject_id)) {
    
    index = PwData$subject_id == id
    print(id)
    
   Add_Power_Spectra = data.frame()
    
    tryCatch(
      expr = {
        
        y = PwData[index, sliders[]]
        y = y[!is.na(y[,1]) & !is.na(y[,2]),]
        # t = c(1:length(y))
        # 
        # A <- (max(y)-min(y))/2
        # C<-(max(y)+min(y))/2
        ssp <- spectrum(y, spans = c(50,50), plot = FALSE) 
        
        Add_Power_Spectra = data.frame(
          freq = ssp$freq, 
          Power_Stimulus = ssp$spec[,1],
          Power_History = ssp$spec[,2],
          Phase = ssp$phase,
          Coherence = ssp$coh)
        
        Add_Power_Spectra$study_id <- unique(PwData[index,]$study_id)
        Add_Power_Spectra$subject_id <- id
      },
      error = function(e){
        Add_Power_Spectra = data.frame(
          freq = NA, 
          Power_Stimulus = NA,
          Power_History = NA,
          Phase = NA,
          Coherence = NA, 
          study_id = NA, 
          subject_id = NA)
        print("error")
      }
    )
    
    
    
    row.names(Add_Power_Spectra) <- NULL
    Power_Spectra = rbind(Power_Spectra, Add_Power_Spectra)
    
  } 
  
  return(Power_Spectra)
}