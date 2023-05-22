f_compute_metacognition = function(PwData) {

  library(nleqslv)
  library(purrr)
  
  Metacognitive = data.frame()
  #for (id in unique(PwData$subject_id[PwData$subject_id > 2240,])) {
  for (id in unique(PwData$subject_id)) {
    index = PwData$subject_id == id
    # print(id)
    
    # Add_Sine_Wave = data.frame(phase = c(NA,NA), omega = c(NA,NA), slider = sliders)
    Add_Metacognitive = data.frame(
      average_internal = NA,
      average_external = NA,
      sensitivity = NA,
      bias = NA,
      study_id = NA,
      subject_id = NA,
      dprime = NA,
      meta_dprime = NA,
      meta_dprime_ratio = NA,
      average_diff_ei = NA,
      var_diff_ei = NA
    )
    
    
    if (sum(!is.na(PwData[index, ]$Confidence)) > 10) {
      ## metacognitive sensitivity
      Add_Metacognitive$sensitivity <-
        glm(Accuracy ~ Confidence, data = PwData[index, ])$coefficients[2]
      Add_Metacognitive$bias <-
        glm(Accuracy ~ Confidence, data = PwData[index, ])$coefficients[1]
      
      
      ## model- based
      SDT_Data <-
        PwData[index, c("Stimulus", "Response", "clear_Confidence")]
      colnames(SDT_Data) <- c("Stimulus", "Response", "Confidence")
        #PwData[index, c("Stimulus", "Response", "Confidence")] old version
      
      SDT_Data <- SDT_Data[!is.na(SDT_Data$Response) & !is.na(SDT_Data$Confidence),]
      
      ## STD2: meta d'
      level_Confidence = sort(unique(SDT_Data$Confidence))
      level_Stimulus = sort(unique(SDT_Data$Stimulus))
      
      n_1 <- double()
      n_2 <- double()
      for (stimulus_idx in c(1:length(level_Stimulus))) {
        Correct = SDT_Data[SDT_Data$Stimulus == level_Stimulus[stimulus_idx] &
                             SDT_Data$Stimulus == SDT_Data$Response , ]
        Incorrect = SDT_Data[SDT_Data$Stimulus == level_Stimulus[stimulus_idx] &
                               SDT_Data$Stimulus != SDT_Data$Response , ]
        n1 <- double()
        n2 <- double()
        for (confidence_idx in c(1:length(level_Confidence))) {
          n1[confidence_idx] <-
            sum(Correct$Confidence == level_Confidence[confidence_idx])
          n2[confidence_idx] <-
            sum(Incorrect$Confidence == level_Confidence[confidence_idx])
        }
        
        if (stimulus_idx == 1) {
          assign(paste("nR_S", stimulus_idx, sep = ""), c(rev(n1), n2))
        } else {
          assign(paste("nR_S", stimulus_idx, sep = ""), c(rev(n2), n1))
        }
      }
      
      
      tryCatch(
        expr = {
          
          STD_2_results <- fit_meta_d_SSE(nR_S1, nR_S2, s = 1, d_min = -5, d_max = 5,
                                          d_grain = .01, add_constant = TRUE)  
          Add_Metacognitive$dprime <- mean(STD_2_results$da, na.rm = TRUE)
          Add_Metacognitive$meta_dprime <- mean(STD_2_results$meta_da, na.rm = TRUE)
          Add_Metacognitive$meta_dprime_ratio <- mean(STD_2_results$M_ratio, na.rm = TRUE)
        },
        error = function(e){
          print("error")
        }
      )
      
      Add_Metacognitive$average_internal <-
        mean(PwData[index, ]$History_slider, na.rm = TRUE)
      Add_Metacognitive$average_external <-
        mean(PwData[index, ]$Accuracy_slider, na.rm = TRUE)
      Add_Metacognitive$average_diff_ei <- mean(PwData[index, ]$Accuracy_slider - PwData[index, ]$History_slider, na.rm = TRUE)
      Add_Metacognitive$var_diff_ei <- sd(PwData[index, ]$Accuracy_slider - PwData[index, ]$History_slider, na.rm = TRUE)
      Add_Metacognitive$study_id <- unique(PwData[index, ]$study_id)
      Add_Metacognitive$subject_id <- id
    }
    
    
    row.names(Add_Metacognitive) <- NULL
    Metacognitive = rbind(Metacognitive, Add_Metacognitive)
    print(mean(Metacognitive$meta_dprime_ratio), na.rm = TRUE)
  }
 
  return(Metacognitive)
}
