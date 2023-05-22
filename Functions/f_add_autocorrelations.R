f_add_autocorrelations <- function(PwData) {
  
  for (id in unique(PwData$subject_id)) {
    index = PwData$subject_id == id
    print(id)
    
    # get trial numbers
    PwData$HardEasy[index] <- round((sign(PwData$Difficulty[index]) + 1)/2)    
    
    
    # get autocorrelations
    acf_HardEasy <-
      acf(
        PwData[index,]$HardEasy,
        lag.max = length(PwData[index,]$HardEasy),
        na.action = na.pass,
        plot = FALSE
      )
    PwData$acf_HardEasy[index] <-  acf_HardEasy$acf
    
    acf_External <-
      acf(
        PwData[index,]$Stimulus,
        lag.max = length(PwData[index,]$Stimulus),
        na.action = na.pass,
        plot = FALSE
      )
    PwData$acf_External[index] <-  acf_External$acf
    
    
    
    
    
    
    Permutations = data.frame()
    for (perm_idx in c(1:n_permutations)){
      addPerm = data.frame()  
      
      random_acf_HardEasy <-
        acf(
          sample(PwData[index,]$HardEasy),
          lag.max = length(PwData[index,]$HardEasy),
          na.action = na.pass,
          plot = FALSE
        )
      
      random_acf_External <-
        acf(
          sample(PwData[index,]$Stimulus),
          lag.max = length(PwData[index,]$Stimulus),
          na.action = na.pass,
          plot = FALSE
        )
      
      addPerm = data.frame(
        random_acf_HardEasy = random_acf_HardEasy$acf,
        exceed_acf_HardEasy = as.integer(PwData$acf_HardEasy[index] < random_acf_HardEasy$acf),
        
        random_acf_External = random_acf_External$acf,
        exceed_acf_External = as.integer(PwData$acf_External[index] < random_acf_External$acf)
      )
      
      addPerm$perm_idx = rep(perm_idx, length(random_acf_HardEasy$acf))
      addPerm$Trial = c(1:nrow(addPerm))
      Permutations = rbind(Permutations, addPerm)
    }
    
    Summ_Permutations <-  ddply(
      Permutations[, ],
      .(Trial),
      summarise,
      
      mean_random_acf_HardEasy = mean(random_acf_HardEasy, na.rm = TRUE),
      sum_exceed_acf_HardEasy = sum(exceed_acf_HardEasy, na.rm = TRUE),
      
      mean_random_acf_External = mean(random_acf_External, na.rm = TRUE),
      sum_exceed_acf_External = sum(exceed_acf_External, na.rm = TRUE)
    )
    
    PwData$random_acf_HardEasy[index] = Summ_Permutations$mean_random_acf_HardEasy
    PwData$exceed_acf_HardEasy[index] = Summ_Permutations$sum_exceed_acf_HardEasy
    
    PwData$random_acf_External[index] = Summ_Permutations$mean_random_acf_External
    PwData$exceed_acf_External[index] = Summ_Permutations$sum_exceed_acf_External
    
  }
  
  return(PwData)
}