simulation_glaze_osc_human_zeta_v5_adaptive <- function(n_blocks, block_length, block_probs, block_precs, var_prec,
                                                        H_logit, P_logit, outcomes, alpha_switch, alpha_stay, alpha_percept, amp, amp_on, frequency, zeta, 
                                                        sliding_window, n_permutations, feedback) {

  ##
  ## generate
  ##
  
  
  ##
  ## generate
  ##
  H_base = H_logit
  P_base = P_logit
  
  Sim = data.frame()
  for (block_idx in c(1:n_blocks)){
    add_Sim = data.frame(
      transitions = rbinom(n=block_length[block_idx], size = 1, prob = block_probs[block_idx]),
      probs = block_probs[block_idx],
      prec = rnorm(n=block_length[block_idx], mean = block_precs[block_idx], sd = var_prec),
      block = block_idx
    )
    Sim = rbind(Sim, add_Sim)
  }
  
  Sim$state = NA
  for (trial_idx in (c(1:nrow(Sim)))){
    
    if (trial_idx == 1){
      Sim$state[trial_idx] = rbinom(n=1, size = 1, prob = 0.5)
      
    } else {
      Sim$state[trial_idx] = abs(Sim$state[trial_idx-1] - Sim$transitions[trial_idx])
      
    }
  }
  
  Sim$u = 1/(1+exp(-Sim$prec*(Sim$state-0.5)))
  Sim$LLR = log(Sim$u/(1-Sim$u))
  Sim$trial = c(1:nrow(Sim))
  Sim$feedback = rbinom(n = nrow(Sim), size = 1, prob = feedback)
  
  ##
  ## simulate 
  ##
  
  Sim$muhat = NA
  Sim$mu= NA
  Sim$H = NA
  Sim$P = NA
  
  Sim$p_transition = NA
  Sim$p_correct = NA
  Sim$da_H = NA
  Sim$da_P = NA
  
  Sim$oscillation_ei = sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1))))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1))))) 
  Sim$oscillation_ei_LLR = sigmoid(-amp*(sin(frequency*(c(1:nrow(Sim))-1))))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1)))))
  
  if (amp_on == 0){
    Sim$oscillation_ei = mean(Sim$oscillation_ei)
    Sim$oscillation_ei_LLR = mean(Sim$oscillation_ei_LLR)
  }
  
  for (k in c(1:nrow(Sim))){
   
    Sim$H[k] = 0.01#sigmoid(H_logit)
    Sim$P[k] = sigmoid(P_logit)
    
    if (k == 1){
      Sim$muhat[k] = 0
    } else {
      Sim$muhat[k] = (Sim$mu[k-1] + log(((1-Sim$H[k])/Sim$H[k]) + exp(-Sim$mu[k-1])) - log( ((1-Sim$H[k])/Sim$H[k]) + exp(Sim$mu[k-1])))
    }
    
    Sim$mu[k] = Sim$muhat[k]*Sim$oscillation_ei[k] + Sim$LLR[k]*Sim$oscillation_ei_LLR[k]
    Sim$da[k] = Sim$mu[k] - Sim$muhat[k]*Sim$oscillation_ei[k]
    Sim$y_prob[k] = sigmoid(zeta*Sim$mu[k])
    Sim$y[k] = outcomes[rbinom(n = 1, size = 1, prob = Sim$y_prob[k]) + 1]
    Sim$certainty[k] = 1-abs(Sim$y[k]-Sim$y_prob[k])
    Sim$correct[k] = -(abs(Sim$y[k] - Sim$state[k])-1)
    
    ## Error H
    if (k > 1){
      Sim$p_transition[k] = abs(Sim$y[k] - Sim$y[k-1])
      
      if (Sim$feedback[k] == 0) {
        Sim$da_H[k] = Sim$p_transition[k] - Sim$H[k] ## H learned based on experience
      } else {
        Sim$da_H[k] = Sim$transitions[k] - Sim$H[k] ## H learned based on feedback
      }
      
      if (Sim$da_H[k] < 0){
        H_logit = H_logit + alpha_stay*Sim$oscillation_ei_LLR[k] * Sim$da_H[k]
      } else {
        H_logit = H_logit + alpha_switch*Sim$oscillation_ei_LLR[k] * Sim$da_H[k]
      }
    }
    
    if (Sim$feedback[k] == 0) {
    Sim$da_P[k] = Sim$certainty[k] - Sim$P[k]
    } else {
    Sim$da_P[k] = Sim$correct[k] - Sim$P[k]
    }
    P_logit = P_logit + alpha_percept*Sim$oscillation_ei_LLR[k] * Sim$da_P[k]
  }
  

  
  Sim$Accuracy = as.numeric((Sim$y == Sim$state))
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
  
  Sim$wLLR = Sim$LLR * Sim$oscillation_ei_LLR
  Sim$wPrior = Sim$muhat * Sim$oscillation_ei
  Sim$Posterior = Sim$mu
  
  Sim$L_Prob = 1-abs(Sim$state-Sim$u)
  
  Sim$error_H = Sim$H - Sim$probs
  Sim$error_P = Sim$P - Sim$L_Prob
  
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
