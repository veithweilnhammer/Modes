f_compute_sim_group_acf <- function(Sim, acf_to_test, max_trial, type_list) {
  
  Sim_Summary_acf <- data.frame()
  for (type_idx in c(1:length(type_list))){
    for (acf_idx in c(1:length(acf_to_test))){
      mean_Y = double()
      sd_Y= double()
      error_Y = double()
      p_Y = character()
      
      for (trial_idx in c(2:max_trial)){
        SimTest <- data.frame()
        Y = double()
        SimTest <- Sim[Sim$Trial == trial_idx & Sim$type == type_list[type_idx], colnames(Sim) %in% c(paste(acf_to_test))]
        assign("Y", SimTest[,acf_to_test[acf_idx]])
        SimTest$Y <- exclude_3SD(Y)
        
        # tryCatch(
        # expr = {
        # 
        # MEM <- summary(lmer(Y ~ 1 + (1|study_id/diff_acf_Difficulty) , data = SimTest))
        # },
        # error = function(e){MEM <- summary(lmer(Y ~ 1 + (1|study_id) , data = SimTest))}
        # )
        Sim_Ttest <- t.test(SimTest$Y)
        #MEM <- summary(lmer(Y ~ 1 + (1|study_id) , data = PwTest))
        
        
        if (Sim_Ttest$p.value< 0.05){
          p_Y[trial_idx-1] = "< 0.05"
        } else {
          p_Y[trial_idx-1] = "> 0.05"
        }
        
        mean_Y[trial_idx-1] = mean(SimTest$Y, na.rm = TRUE)
        sd_Y[trial_idx-1] =  sd(SimTest$Y, na.rm = TRUE)
        error_Y[trial_idx-1] = qnorm(0.975)*sd_Y[trial_idx-1]/sqrt(nrow(SimTest))
      }
      
      add_Sim_Summary_acf = data.frame(
        Trial = c(2:max_trial),
        Variable = rep(acf_to_test[acf_idx], max_trial-1),
        type = rep(type_list[type_idx], max_trial-1),
        Mean = c(mean_Y), 
        Sd = c(sd_Y),
        Error = c(error_Y),
        p = rep(p_Y)
      )
      Sim_Summary_acf = rbind(Sim_Summary_acf, add_Sim_Summary_acf)
    }
  }
  
  return(Sim_Summary_acf)
}