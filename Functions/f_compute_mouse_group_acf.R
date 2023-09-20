f_compute_mouse_group_acf <- function(PwData, acf_to_test, max_trial) {
  
  Summary_acf = data.frame()
  
  for (acf_idx in c(1:length(acf_to_test))){
    mean_Y = double()
    sd_Y= double()
    error_Y = double()
    p_Y = character()
    
    for (trial_idx in c(2:max_trial)){
      print(trial_idx)
      PwTest <- data.frame()
      Y = double()
      PwTest <- PwData[PwData$Trial == trial_idx , colnames(PwData) %in% c(paste(acf_to_test[acf_idx]), 'diff_acf_External', 'diff_acf_Difficulty', 'session_id')]
      assign("Y", PwTest[,acf_to_test[acf_idx]])
      PwTest$Y <- exclude_3SD(Y)

      #MEM <- summary(lmer(Y ~ 1 + diff_acf_External + (1|session_id) , data = PwTest))
      MEM <- summary(lmer(Y ~ 1 + diff_acf_External + diff_acf_Difficulty + (1|session_id), data = PwTest))
      
      if (MEM$coefficients[13] < 0.05){
        p_Y[trial_idx-1] ="< 0.05"
      } else {
        p_Y[trial_idx-1] = "> 0.05"
      }
      
      # MEM <- summary(lmer(Y ~ 1 + (1|session_id) , data = PwTest))
      # 
      # if (MEM$coefficients[5] < 0.05){
      #   p_Y[trial_idx-1] ="< 0.05"
      # } else {
      #   p_Y[trial_idx-1] = "> 0.05"
      # }
      
      mean_Y[trial_idx-1] = mean(PwTest$Y, na.rm = TRUE)
      sd_Y[trial_idx-1] =  sd(PwTest$Y, na.rm = TRUE)
      error_Y[trial_idx-1] = qnorm(0.975)*sd_Y[trial_idx-1]/sqrt(nrow(PwTest))
    }
    
    add_Summary_acf = data.frame(
      Trial = c(2:max_trial),
      Variable = rep(acf_to_test[acf_idx], max_trial-1),
      Mean = c(mean_Y), 
      Sd = c(sd_Y),
      Error = c(error_Y),
      p = rep(p_Y)
    )
    Summary_acf = rbind(Summary_acf, add_Summary_acf)
    
  }
  
  return(Summary_acf)
}