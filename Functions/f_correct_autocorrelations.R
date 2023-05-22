f_correct_autocorrelations <- function(PwData) {
  
  for (id in unique(PwData$subject_id)) {
    index = PwData$subject_id == id
    print(id)
    
    acf_History <-
      acf(
        PwData[index,]$History,
        lag.max = length(PwData[index,]$History),
        na.action = na.pass,
        plot = FALSE
      )
    PwData$acf_History[index] = acf_History$acf
    
    
    Permutations = data.frame()
    for (perm_idx in c(1:n_permutations)){
      addPerm = data.frame()  
      
      random_acf_History <-
        acf(
          sample(PwData[index,]$History),
          lag.max = length(PwData[index,]$History),
          na.action = na.pass,
          plot = FALSE
        )
      
    
      
      addPerm = data.frame(
        random_acf_History = random_acf_History$acf,
        exceed_acf_History = as.integer(PwData$acf_History[index] < random_acf_History$acf)
      )
      
      addPerm$perm_idx = rep(perm_idx, length(random_acf_History$acf))
      addPerm$Trial = c(1:nrow(addPerm))
      Permutations = rbind(Permutations, addPerm)
    }
    
    Summ_Permutations <-  ddply(
      Permutations[, ],
      .(Trial),
      summarise,
      
      mean_random_acf_History = mean(random_acf_History, na.rm = TRUE),
      sum_exceed_acf_History = sum(exceed_acf_History, na.rm = TRUE)
    )
    
    PwData$random_acf_History[index] = Summ_Permutations$mean_random_acf_History
    PwData$exceed_acf_History[index] = Summ_Permutations$sum_exceed_acf_History
    
    
  }
  
  return(PwData)
}