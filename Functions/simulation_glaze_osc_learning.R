simulation_glaze_osc_learning <- function(outcomes, n, H_true, H_est, prec_true, prec_est, amp, amp_LLR, frequency, phase) {

  Sim = data.frame(changes = rep(NA, n), env = rep(NA, n), u = rep(NA, n), LLR = rep(NA,n), muhat = rep(NA, n), mu = rep(NA, n), y_prob = rep(NA, n), y = rep(NA, n), da = rep(NA, n), oscillation_ei = rep(NA, n), oscillation_ei_LLR = rep(NA, n), H_est = rep(NA,n), prec_est = rep(NA,n), Trial = c(1:n))
  
  ##
  ## generate changes in the environment
  ##
  Sim$changes = -(rbinom(n=n, size = 1, prob = H_true)*2 - 1 ) 
  
  ##
  ## Oscillations
  ##
  Sim$oscillation_ei = (amp*(sin(frequency*(c(1:n)-1)))) + 1
  Sim$oscillation_ei_LLR = (amp_LLR*(sin(frequency*(c(1:n)-1) + phase))) + 1
  
  for (k in c(1:n)){
    
    if (k == 1)
    {Sim$env[k] = (rbinom(n=1, size = 1, prob = 0.5)*2 - 1) * runif(1, 0.2, 1)
    Sim$H_est[k] = H_est
    Sim$prec_est[k] = prec_est
    } else {
      Sim$env[k] = (sign(Sim$env[k-1])*Sim$changes[k])*runif(1, 0.2, +1)
      Sim$H_est[k] = Sim$H_est[k - 1]
      
      Sim$prec_est[k] = Sim$prec_est[k - 1]
    }
    
    Sim$u[k] = sigmoid(prec_true*Sim$env[k])
    
    Sim$LLR[k] = log(Sim$u[k]/(1-Sim$u[k]))
    
    if (k == 1){
      Sim$muhat[k] = 0
    } else {
      Sim$muhat[k] = (Sim$mu[k-1] + log( ((1- Sim$H_est[k])/Sim$H_est[k]) + exp(-Sim$mu[k-1])) - log( ((1- Sim$H_est[k])/ Sim$H_est[k]) + exp(Sim$mu[k-1])))*Sim$oscillation_ei[k]
    }
    
    Sim$mu[k] = Sim$muhat[k] + Sim$LLR[k]*Sim$oscillation_ei_LLR[k]
    
    Sim$y_prob[k] = exp(Sim$mu[k])/(1 +  exp(Sim$mu[k]))
    Sim$y[k] = outcomes[rbinom(n = 1, size = 1, prob = Sim$y_prob[k]) + 1]
  }
  
  Sim$Accuracy = as.numeric((Sim$y == round(Sim$u)))
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
  Sim$Accuracy_slider <- Sim$Accuracy_slider * 100
  Sim$History_slider <- Sim$History_slider * 100
  Sim$Mode = Sim$Accuracy_slider - Sim$History_slider
  
  
  return(Sim)
  
}
