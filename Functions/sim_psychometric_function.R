sim_psychometric_function <- function(par, Input_Data) {   
  
  lower_lapse = par[1]
  higher_lapse = par[2]
  bias = par[3]
  contrast_threshold = par[4]
  
  Input_Data$Difficulty = Input_Data$rDifficulty - min(Input_Data$rDifficulty) + 1
  
  Input_Data$dStimulus <-
    ((Input_Data$Stimulus - 0.5) * 2 * Input_Data$Difficulty)
  
  y_prob = rep(NA, nrow(Input_Data))
  
  y_prob = lower_lapse + (1 - lower_lapse - higher_lapse) *  (erf((Input_Data$dStimulus -
                                                                     bias) / contrast_threshold) + 1) / 2
  

  accuracy_term = Input_Data$Response*log(y_prob) + (1-Input_Data$Response) * log(1-y_prob)
  sum_Error = -sum(accuracy_term[is.finite(accuracy_term)])
  
  return(y_prob)
}
