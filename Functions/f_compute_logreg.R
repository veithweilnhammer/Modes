f_compute_logreg = function(PwData) {

  LogAggr = data.frame()  
  for (id in unique(PwData$subject_id)) {
    index = PwData$subject_id == id
    print(id)
    
    full_logit = data.frame(aic = NA, coefficients = c(NA,NA,NA))
    reduced_logit = data.frame(aic = NA)
    
    tryCatch(
      expr = {
        LogData <- PwData[index, c("Response", "Stimulus", "Response_minus_1")]
        
        if (min(LogData, na.rm = TRUE) != 0) {
          
          LogData <- LogData - min(LogData, na.rm = TRUE)
        }
        
        full_logit <-
          glm(Response ~ Stimulus + Response_minus_1,
              data = LogData, na.action = na.omit,
              family = "binomial")
        
        reduced_logit <-
          glm(Response ~ Stimulus,
              data = LogData, na.action = na.omit,
              family = "binomial")
      },
      error = function(e){
        full_logit$aic = NA
        reduced_logit$aic = NA
        full_logit$coefficients = c(NA,NA,NA)}
    )
    
    Add_logReg = data.frame()
    Add_logReg = data.frame(subject_id = id,
                            study_id = PwData[index, ]$study_id[1],
                            full_AIC = full_logit$aic,
                            reduced_AIC = reduced_logit$aic,
                            beta_Stimulus = full_logit$coefficients[2],
                            beta_History = full_logit$coefficients[3])
    
    row.names(Add_logReg) <- NULL
    LogAggr = rbind(LogAggr, Add_logReg)
    
  }

  return(LogAggr)
}
