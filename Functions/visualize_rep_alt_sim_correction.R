visualize_rep_alt_sim_correction <- function(blocks, n_trials_per_block) {   
  history_sequence = c()
  for (block_idx in c(1:length(blocks))){
  history_sequence = history_sequence = c(history_sequence, rbinom(n=n_trials_per_block[block_idx], size = 1, prob = blocks[block_idx]))
  }
  acf_History_Bias <-
    acf(
      history_sequence,
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
    trial = 1:length(acf_History_Bias$acf)
  )
  
  
  return(Bias_Sim)
}