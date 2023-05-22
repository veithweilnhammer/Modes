fit_glaze_osc_zeta_v4_no_integration <- function(par, Input_Data) {   
  
  H = 0.5
  prec = par[1]
  zeta = 1#par[2]
  
  # Fit <- data.frame(
  #   env = Input_Data$Stimulus - min(Input_Data$Stimulus),
  #   response = Input_Data$Response - min(Input_Data$Response)
  # )
  
  Fit <- data.frame(
    env = Input_Data$Stimulus,
    response = Input_Data$Response
  )
  max_env = which(Fit$env == max(Fit$env, na.rm = TRUE))
  min_env = which(Fit$env == min(Fit$env, na.rm = TRUE))
  Fit$env[max_env] = 1
  Fit$env[min_env] = 0
  
  max_response = which(Fit$response == max(Fit$response, na.rm = TRUE))
  min_response = which(Fit$response == min(Fit$response, na.rm = TRUE))
  Fit$response[max_response] = 1
  Fit$response[min_response] = 0
  
  #Fit$u = 0.5 + (Fit$env - 0.5)*(prec)
  #Fit$u = sigmoid((prec)*Fit$env*2 - 1)
  Fit$u = 1/(1+exp(-prec*(Fit$env-0.5)))
  Fit$LLR = log(Fit$u/(1-Fit$u))
  
  #Fit$oscillation_ei = sigmoid(amp*(sin(frequency*(c(1:nrow(Fit))-1) + phase)))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Fit))-1)))))
  #Fit$oscillation_ei_LLR = sigmoid(-amp*(sin(frequency*(c(1:nrow(Fit))-1) + phase)))/max(sigmoid(amp*(sin(frequency*(c(1:nrow(Fit))-1)))))
  
  n = nrow(Fit)
  for (k in c(1:n)){
    
    if (k == 1){
      
      Fit$muhat[k] = 0
      
    } else {
      Fit$muhat[k] = (Fit$mu[k-1] + log( ((1-H)/H) + exp(-Fit$mu[k-1])) - log( ((1-H)/H) + exp(Fit$mu[k-1])))#*Fit$oscillation_ei[k]
    }
    
    Fit$mu[k] = Fit$muhat[k] + Fit$LLR[k]#*Fit$oscillation_ei_LLR[k]
    # Fit$mu[k] = Fit$muhat[k] + Fit$LLR[k]/Fit$oscillation_ei[k]
    Fit$da[k] = Fit$mu[k] - Fit$muhat[k]
    #Fit$y_prob[k] = exp(Fit$mu[k])/(1 +  exp(Fit$mu[k]))
    Fit$y_prob[k] = sigmoid(zeta*Fit$mu[k])
  }
  
  # logx = log(Fit$y_prob)
  # log1pxm1 = log1p(Fit$y_prob-1)
  # logx[1-Fit$y_prob<1e-4] = log1pxm1[1-Fit$y_prob<1e-4]
  # log1mx = log(1-Fit$y_prob)
  # log1pmx = log1p(-Fit$y_prob)
  # log1mx[Fit$y_prob<1e-4] = log1pmx[Fit$y_prob<1e-4] 
  # 
  # logp = Fit$response*(logx - log1mx) + log1mx -log((1-Fit$y_prob) +Fit$y_prob)
  
 # sum_Error =  sum((Fit$response - Fit$y_prob)^2, na.rm = TRUE)
  
  ## log loss/cross entropy
  accuracy_term = Fit$response*log(Fit$y_prob) + (1-Fit$response) * log(1-Fit$y_prob)
  #sum_Error = (-sum(accuracy_term[is.finite(accuracy_term)]))/length(accuracy_term[is.finite(accuracy_term)])
  sum_Error = -sum(accuracy_term[is.finite(accuracy_term)])
  
  # print(sum_Error)
  return(sum_Error)
}
