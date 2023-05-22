visualize_bias_correction <- function(n_trials, level) {   
  
  sequence = rbinom(n=n_trials, size = 1, prob = level)
  sequence_minus_1 = c(NA, sequence[1:length(sequence) - 1])
  history_sequence = as.numeric(sequence == sequence_minus_1)
  
  acf_History_Bias <-
    acf(
      sequence,
      lag.max = length(history_sequence),
      na.action = na.pass,
      plot = FALSE
    )
  
  # get random autocorrelations
  Permutations = data.frame()
  for (perm_idx in c(1:n_permutations)){
    addPerm = data.frame()  
    
    
    random_acf_History_Bias <-
      acf(
        sample(history_sequence),
        lag.max = length(history_sequence),
        na.action = na.pass,
        plot = FALSE
      )
    
    
    
    addPerm = data.frame(
      random_acf_History_Bias = random_acf_History_Bias$acf)
    
    
    addPerm$perm_idx = rep(perm_idx, length(random_acf_History_Bias$acf))
    addPerm$Trial = c(1:nrow(addPerm))
    Permutations = rbind(Permutations, addPerm)
  }
  
  library(plyr)
  Summ_Permutations <-  ddply(
    Permutations[, ],
    .(Trial),
    summarise,
    
    mean_random_acf_History_Bias = mean(random_acf_History_Bias, na.rm = TRUE))
  
  
  Bias_Sim = data.frame(
    acf = acf_History_Bias$acf,
    random = Summ_Permutations$mean_random_acf_History_Bias,
    bias_level = level*100,
    trial = 1:n_trials
  )
  
  
  return(Bias_Sim)
}