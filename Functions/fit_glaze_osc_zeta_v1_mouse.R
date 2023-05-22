fit_glaze_osc_zeta_v1_mouse <- function(par, Input_Data) {   
  
  H = par[1]
  prec = par[2]
  amp = par[3]
  amp_LLR = par[4]
  frequency = par[5]
  phase = par[6]
  zeta = par[7]
  
  # Fit <- data.frame(
  #   env = Input_Data$Stimulus - min(Input_Data$Stimulus),
  #   response = Input_Data$Response - min(Input_Data$Response)
  # )
  
  Fit <- data.frame(
    env = Input_Data$Stimulus,
    response = Input_Data$Response
  )
  
  Fit$u = sigmoid((prec)*Fit$env)
  Fit$LLR = log(Fit$u/(1-Fit$u))
  
  Fit$oscillation_ei = (amp*(sin(frequency*(c(1:nrow(Fit))-1) + phase))) + 1
  Fit$oscillation_ei_LLR = (amp_LLR*(sin(frequency*(c(1:nrow(Fit))-1) + phase + pi))) + 1
  
  n = nrow(Fit)
  for (k in c(1:n)){
    
    if (k == 1){
      
      Fit$muhat[k] = 0
      
    } else {
      Fit$muhat[k] = (Fit$mu[k-1] + log( ((1-H)/H) + exp(-Fit$mu[k-1])) - log( ((1-H)/H) + exp(Fit$mu[k-1])))*Fit$oscillation_ei[k]
    }
    
    Fit$mu[k] = Fit$muhat[k] + Fit$LLR[k]*Fit$oscillation_ei_LLR[k]
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
