f_compute_mouse_trial_wise_logreg = function(MwData, n_back) {

  LogAggr = data.frame()  
  for (id in unique(MwData$subject_id)) {
    for (session_idx in unique(MwData[MwData$subject_id == id,]$session_id)){
      index = (MwData$subject_id == id & MwData$session_id == session_idx)
      
      if (sum(index) > n_back){
        
        if ((id %% 10) == 0){print(id)}
        
        History_Data <- MwData[index, c("session_id", "subject_id", "History")]
        Stimulus_Data <- MwData[index, c("session_id", "subject_id", "Accuracy")]
        for (lag_idx in c(1:n_back)){
          History_Data[, 4] =  (c(1:nrow(History_Data))-lag_idx) 
          History_Data[History_Data[, 4] < 1, 4] =  NA
          History_Data[,4] =  History_Data$History[History_Data[,4]]
          
          History_logit <-
            glm(History ~ V4,
                data = History_Data, na.action = na.omit,
                family = "binomial")
          
          History_logit_zero <-
            glm(History ~ 1,
                data = History_Data, na.action = na.omit,
                family = "binomial")
          
          Stimulus_Data[, 4] =  (c(1:nrow(Stimulus_Data))-lag_idx) 
          Stimulus_Data[Stimulus_Data[, 4] < 1, 4] =  NA
          Stimulus_Data[,4] =  Stimulus_Data$Accuracy[Stimulus_Data[,4]]
          
          Stimulus_logit <-
            glm(Accuracy ~ V4,
                data = Stimulus_Data, na.action = na.omit,
                family = "binomial")
          
          Stimulus_logit_zero <-
            glm(Accuracy ~ 1,
                data = Stimulus_Data, na.action = na.omit,
                family = "binomial")
          
          add_logReg = data.frame(session_id = unique(History_Data$session_id),
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
  }

  return(LogAggr)
}
