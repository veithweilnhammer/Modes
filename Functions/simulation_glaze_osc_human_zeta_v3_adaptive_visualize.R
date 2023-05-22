simulation_glaze_osc_human_zeta_v3_adaptive_visualize <- function(n_blocks, block_length, block_probs, block_precs, var_prec,
                                                        H_logit, P_logit, outcomes, alpha_switch, alpha_stay, alpha_percept, amp, frequency, zeta, 
                                                        sliding_window, n_permutations) {

  ##
  ## generate
  ##
  H_base = H_logit
  P_base = P_logit
  
  Sim = data.frame()
  for (block_idx in c(1:n_blocks)){
    
    add_Sim = data.frame(
      transitions = rbinom(n=block_length, size = 1, prob = block_probs[block_idx]),
      probs = block_probs[block_idx],
      prec = rnorm(n=block_length, mean = block_precs[block_idx], sd = var_prec),
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
  
  Sim$amp_on = 1
  

  Sim$oscillation_ei = sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1))))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1)))))
  Sim$oscillation_ei_LLR = sigmoid(-amp*(sin(frequency*(c(1:nrow(Sim))-1))))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Sim))-1)))))
  
  Sim_0 = Sim
  
  Sim_0$amp_on = 0
  
    Sim_0$oscillation_ei = mean(Sim$oscillation_ei)
    Sim_0$oscillation_ei_LLR = mean(Sim$oscillation_ei_LLR)
  
  
  
  Sim <- rbind(Sim_0, Sim)
  
  
  
  
  for (k in c(1:nrow(Sim))){
    if ( (k ==1) | (k == nrow(Sim)/2 + 1)){
      H_logit = H_base
      P_logit = P_base}
    
    Sim$H[k] = sigmoid(H_logit)
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
    Sim$correct[k] = -(abs(Sim$y[k] - Sim$state[k])-1)
    
    ## Error H
    if (k > 1){
      Sim$p_transition[k] = abs(Sim$y[k] - Sim$y[k-1])
      #Sim$da_H[k] = Sim$p_transition[k] - Sim$H[k] ## H learned based on experience
      Sim$da_H[k] = Sim$transitions[k] - Sim$H[k] ## H learned based on feedback
      
      if (Sim$da_H[k] < 0){
        H_logit = H_logit + alpha_stay*Sim$oscillation_ei[k] * Sim$da_H[k]
      } else {
        H_logit = H_logit + alpha_switch*Sim$oscillation_ei[k] * Sim$da_H[k]
      }
    }
    
    Sim$da_P[k] = Sim$correct[k] - Sim$P[k]
    P_logit = P_logit + alpha_percept*Sim$oscillation_ei_LLR[k] * Sim$da_P[k]
  }
  
  # for (k in c(1:nrow(Sim))){
  #   
  #   Sim$H[k] = sigmoid(H_logit)
  #  
  #    if (k == 1){
  #     Sim$muhat[k] = 0
  #     Sim$mu[k] = Sim$muhat[k] + Sim$LLR[k]
  # 
  #   } else {
  #     Sim$muhat[k] = (Sim$mu[k-1] + log(((1-Sim$H[k])/Sim$H[k]) + exp(-Sim$mu[k-1])) - log( ((1-Sim$H[k])/Sim$H[k]) + exp(Sim$mu[k-1])))
  #     Sim$mu[k] = Sim$muhat[k] + Sim$LLR[k]*abs(Sim$da[k-1])
  #  
  #   }
  #   
  #   Sim$mu[k] = Sim$muhat[k]*Sim$oscillation_ei[k] + Sim$LLR[k]*Sim$oscillation_ei_LLR[k]
  #   Sim$da[k] = Sim$mu[k] - Sim$muhat[k]
  #   Sim$y_prob[k] = sigmoid(zeta*Sim$mu[k])
  #   Sim$y[k] = outcomes[rbinom(n = 1, size = 1, prob = Sim$y_prob[k]) + 1]
  #   
  #   if (k > 1){
  #     Sim$p_transition[k] = abs(Sim$y[k] - Sim$y[k-1])
  #     Sim$da_H[k] = Sim$p_transition[k] - Sim$H[k]
  #     if (Sim$da_H[k] < 0){
  #     H_logit = H_logit + alpha_stay * Sim$da_H[k]
  #     } else {
  #     H_logit = H_logit + alpha_switch * Sim$da_H[k]
  #     }
  #   }
  # }
  
  
  Sim$Accuracy = as.numeric((Sim$y == Sim$state))
  Sim$History = as.numeric(c(NA, diff(Sim$y) == 0))
  Sim$error_H = Sim$H - Sim$probs
  
  
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
  
  
  
  return(Sim)
  
}
