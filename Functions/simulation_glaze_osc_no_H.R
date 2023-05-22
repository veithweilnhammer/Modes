simulation_glaze_osc_no_H <- function(outcomes, n, H, prec, amp, frequency, phase, random_steps, sliding_window, n_permutations, type) {
  
  Sim = data.frame(env = rep(NA, n), u = rep(NA, n), LLR = rep(NA,n), muhat = rep(NA, n), mu = rep(NA, n), y_prob = rep(NA, n), y = rep(NA, n), da = rep(NA, n), oscillation_ei = rep(NA, n))
  

  Sim$oscillation_ei = (1.2*amp*(sin(frequency*(c(1:n)-1)))) + 1
  Sim$oscillation_ei_LLR = (0.4*amp*(sin(frequency*(c(1:n)-1) + phase))) + 1
  
  # Sim$oscillation_ei[Sim$oscillation_ei <= 0] = 0;
  H_base = H
  prec_base = prec
  for (k in c(1:n)){
    H = H_base# + Sim$oscillation_ei[k]
    prec = prec_base# - Sim$oscillation_ei[k]

    Sim$env[k] = rbinom(n=1, size = 1, prob = 0.5)
    
    Sim$u[k] = 0.5 + (Sim$env[k] - 0.5)*(prec + random_steps[k])
    
    Sim$LLR[k] = log(Sim$u[k]/(1-Sim$u[k]))
    
    if (k == 1){
      
      Sim$muhat[k] = 0
      
    } else {
      Sim$muhat[k] = (Sim$mu[k-1] + log( ((1-H)/H) + exp(-Sim$mu[k-1])) - log( ((1-H)/H) + exp(Sim$mu[k-1])))*Sim$oscillation_ei[k]
    }
    
    Sim$mu[k] = Sim$muhat[k] + Sim$LLR[k]*Sim$oscillation_ei_LLR[k]
    # Sim$mu[k] = Sim$muhat[k] + Sim$LLR[k]/Sim$oscillation_ei[k]
    Sim$da[k] = Sim$mu[k] - Sim$muhat[k]
    Sim$y_prob[k] = exp(Sim$mu[k])/(1 +  exp(Sim$mu[k]))
    Sim$y[k] = outcomes[rbinom(n = 1, size = 1, prob = Sim$y_prob[k]) + 1]
  }
  
  
  Sim$Accuracy = as.numeric((Sim$y == Sim$env))
  Sim$History = as.numeric(c(NA, diff(Sim$y) == 0))
  
  
  # get slider
  Sim$History_slider = rep(NA, length(Sim$History))
  Sim$Accuracy_slider = rep(NA, length(Sim$Accuracy))
  
  # slider
  for (slider in (1):length(Sim$History_slider)) {
    #sliding_index = (slider - sliding_window + 1):slider
    sliding_index = (slider - sliding_window / 2):(slider + sliding_window /
                                                     2)
    sliding_index <-
      sliding_index[sliding_index > 0 &
                      sliding_index <= length(Sim$History_slider)]
    
    Sim$History_slider[slider] <-
      sum(Sim$History[sliding_index], na.rm = TRUE) /
      length(sliding_index)
    
    Sim$Accuracy_slider[slider] <-
      sum(Sim$Accuracy[sliding_index], na.rm = TRUE) /
      length(sliding_index)
  }
  
  
  # get autocorrelations
  acf_Stimulus <-
    acf(
      Sim$Accuracy,
      lag.max = length(Sim$Accuracy),
      na.action = na.pass,
      plot = FALSE
    )
  
  acf_History <-
    acf(
      Sim$History,
      lag.max = length(Sim$History),
      na.action = na.pass,
      plot = FALSE
    )
  
  # get random autocorrelations
  Permutations = data.frame()
  for (perm_idx in c(1:n_permutations)){
    addPerm = data.frame()  
    
    
    random_acf_Stimulus <-
      acf(
        sample(Sim$Accuracy),
        lag.max = length(Sim$Accuracy),
        na.action = na.pass,
        plot = FALSE
      )
    
    random_acf_History <-
      acf(
        sample(Sim$History),
        lag.max = length(Sim$History),
        na.action = na.pass,
        plot = FALSE
      )
    
    addPerm = data.frame(
      random_acf_Stimulus = random_acf_Stimulus$acf,
      random_acf_History = random_acf_History$acf,
      exceed_acf_Stimulus = as.integer(acf_Stimulus$acf < random_acf_Stimulus$acf),
      exceed_acf_History = as.integer(acf_History$acf < random_acf_History$acf))
    
    
    addPerm$perm_idx = rep(perm_idx, length(random_acf_Stimulus$acf))
    addPerm$Trial = c(1:nrow(addPerm))
    Permutations = rbind(Permutations, addPerm)
  }
  
  Summ_Permutations <-  ddply(
    Permutations[, ],
    .(Trial),
    summarise,
    
    mean_random_acf_Stimulus = mean(random_acf_Stimulus, na.rm = TRUE),
    mean_random_acf_History = mean(random_acf_History, na.rm = TRUE),
    
    sum_exceed_acf_Stimulus = sum(exceed_acf_Stimulus, na.rm = TRUE),
    sum_exceed_acf_History = sum(exceed_acf_History, na.rm = TRUE))
  
  
  Sim$acf_Stimulus <- acf_Stimulus$acf
  Sim$acf_History <- acf_History$acf
  
  Sim$random_acf_Stimulus <- Summ_Permutations$mean_random_acf_Stimulus
  Sim$random_acf_History <- Summ_Permutations$mean_random_acf_History
  
  Sim$exceed_acf_History = Summ_Permutations$sum_exceed_acf_History
  Sim$exceed_acf_Stimulus = Summ_Permutations$sum_exceed_acf_Stimulus
  
  return(Sim)
  
}
