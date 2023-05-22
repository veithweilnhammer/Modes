f_compute_trial_wise_logreg = function(PwData, n_back) {

  LogAggr = data.frame()  
  for (id in unique(PwData$subject_id)) {
    index = PwData$subject_id == id
    
    if ((id %% 10) == 0){print(id)}
    History_Data <- PwData[index, c("study_id", "subject_id", "History", "Difficulty", "Stimulus")]
    History_Data$Stimulus_rep = abs(abs(c(NA, diff(History_Data$Stimulus)))-1)
    
    Stimulus_Data <- PwData[index, c("study_id", "subject_id", "Accuracy", "Difficulty", "Stimulus")]
    Stimulus_Data$Stimulus_rep = abs(abs(c(NA, diff(Stimulus_Data$Stimulus)))-1)
    
    for (lag_idx in c(1:n_back)){
      History_Data[, 7] =  (c(1:nrow(History_Data))-lag_idx) 
      History_Data[History_Data[, 7] < 1, 7] =  NA
      History_Data[,7] =  History_Data$History[History_Data[,7]]
      
      Stimulus_Data[, 7] =  (c(1:nrow(Stimulus_Data))-lag_idx) 
      Stimulus_Data[Stimulus_Data[, 7] < 1, 7] =  NA
      Stimulus_Data[,7] =  Stimulus_Data$Accuracy[Stimulus_Data[,7]]
     
      
      if (sum(!is.na(History_Data$V7)) & sum(!is.na(Stimulus_Data$V7))){
        if (!is.na(History_Data$Difficulty)){
         History_logit <-
        glm(History ~ V7 + Difficulty + Stimulus_rep,
            data = History_Data, na.action = na.omit,
            family = "binomial")
      
        History_logit_zero <-
        glm(History ~ 1 + Difficulty + Stimulus_rep,
            data = History_Data, na.action = na.omit,
            family = "binomial")
        
        Stimulus_logit <-
        glm(Accuracy ~ V7 + Difficulty + Stimulus_rep,
            data = Stimulus_Data, na.action = na.omit,
            family = "binomial")
      
        Stimulus_logit_zero <-
        glm(Accuracy ~ 1 + Difficulty + Stimulus_rep,
            data = Stimulus_Data, na.action = na.omit,
            family = "binomial")
        
      } else {
        History_logit <-
          glm(History ~ V7 +  Stimulus_rep,
              data = History_Data, na.action = na.omit,
              family = "binomial")
        
        History_logit_zero <-
          glm(History ~ 1 +  Stimulus_rep,
              data = History_Data, na.action = na.omit,
              family = "binomial")
        
        Stimulus_logit <-
          glm(Accuracy ~ V7 +  Stimulus_rep,
              data = Stimulus_Data, na.action = na.omit,
              family = "binomial")
        
        Stimulus_logit_zero <-
          glm(Accuracy ~ 1 +  Stimulus_rep,
              data = Stimulus_Data, na.action = na.omit,
              family = "binomial")
      }
     
 
      
      
      
      add_logReg = data.frame(study_id = unique(History_Data$study_id),
                              subject_id = unique(History_Data$subject_id),
                              lag = lag_idx,
                              History_weight = History_logit$coefficients[2],
                              History_diff_AIC = History_logit_zero$aic - History_logit$aic,
                              Stimulus_weight = Stimulus_logit$coefficients[2],
                              Stimulus_diff_AIC = Stimulus_logit_zero$aic - Stimulus_logit$aic
      )
      row.names(add_logReg) <- NULL
      LogAggr = rbind(LogAggr, add_logReg) 
      }
      
    }
    
  }

  return(LogAggr)
}
