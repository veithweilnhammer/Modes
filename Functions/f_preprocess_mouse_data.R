f_preprocess_mouse_data <- function(loc, n_permutations) {
  
  RawData <- read.csv(loc)
  
  library(plyr)
  
  MwData = data.frame()
  
  subject_counter = 0
  for (subject_id in unique(RawData$subject_uuid)) {
    subject_counter = subject_counter + 1
    
    print(subject_counter)
    
    session_counter = 0
    for (session_id in unique(RawData[RawData$subject_uuid == subject_id,]$session_start_time)) {
      session_counter = session_counter + 1
      
      index = RawData$subject_uuid == subject_id &  RawData$session_start_time == session_id
      Data = RawData[index,]
      
      Data$Stimulus = NA
      Data$Stimulus[(Data$trial_stim_contrast_left == 0 & Data$trial_stim_contrast_right != 0)] = 1
      Data$Stimulus[(Data$trial_stim_contrast_left != 0 & Data$trial_stim_contrast_right == 0)] = 0
      # Data$Stimulus[(Data$trial_stim_contrast_left == 0 & Data$trial_stim_contrast_right == 0)] = 0.5
      
      Data$Response = NA
      Data$Response[(Data$trial_response_choice == "CCW")] = 1
      Data$Response[(Data$trial_response_choice == "CW")] = 0
      
      Data$Response_minus_1 <- c(NA, Data$Response[1:nrow(Data) - 1])
      Data$Stimulus_minus_1 <- c(NA, Data$Stimulus[1:nrow(Data) - 1])
      
      Data$History = as.numeric(Data$Response == Data$Response_minus_1)
      Data$Stimulus_History = as.numeric(Data$Stimulus == Data$Stimulus_minus_1)
      Data$Accuracy = as.numeric(Data$Stimulus == Data$Response)
      
      Data$RT = NA
      Data$RT = Data$trial_response_time - Data$trial_start_time
      
      Data$Difficulty = NA
      Data$Difficulty = abs(Data$trial_stim_contrast_left - Data$trial_stim_contrast_right)
      
      Data$HardEasy <- round((-sign(scale(abs(Data$trial_stim_contrast_left - Data$trial_stim_contrast_right))) + 1)/2)    
      
      
      
      
      # get trial numbers
      Data$Trial = 1:nrow(Data)
      
      # get autocorrelations
      acf_Stimulus <-
        acf(
          Data$Accuracy,
          lag.max = length(Data$Accuracy),
          na.action = na.pass,
          plot = FALSE
        )
      
      acf_History <-
        acf(
          Data$History,
          lag.max = length(Data$History),
          na.action = na.pass,
          plot = FALSE
        )
      
      
      acf_Perception <-
        acf(
          Data$Response,
          lag.max = length(Data$Response),
          na.action = na.pass,
          plot = FALSE
        )
      
      acf_Difficulty <-
        acf(
          Data$Difficulty,
          lag.max = length(Data$Difficulty),
          na.action = na.pass,
          plot = FALSE
        )
      
      acf_HardEasy <-
        acf(
          Data$HardEasy,
          lag.max = length(Data$HardEasy),
          na.action = na.pass,
          plot = FALSE
        )
      
      acf_RT <-
        acf(
          Data$RT,
          lag.max = length(Data$RT),
          na.action = na.pass,
          plot = FALSE
        )
      
      acf_External <-
        acf(
          Data$Stimulus,
          lag.max = length(Data$Stimulus),
          na.action = na.pass,
          plot = FALSE
        )
      
      Data$acf_History = acf_History$acf
      Data$acf_Stimulus = acf_Stimulus$acf
      Data$acf_Perception = acf_Perception$acf
      Data$acf_RT = acf_RT$acf
      Data$acf_Difficulty = acf_Difficulty$acf
      Data$acf_HardEasy = acf_HardEasy$acf
      Data$acf_External = acf_External$acf
      
      
      Permutations = data.frame()
      for (perm_idx in c(1:n_permutations)){
        addPerm = data.frame()  
        
        random_acf_Stimulus <-
          acf(
            sample(Data$Accuracy),
            lag.max = length(Data$Accuracy),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_History <-
          acf(
            sample(Data$History),
            lag.max = length(Data$History),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_RT <-
          acf(
            sample(Data$RT),
            lag.max = length(Data$RT),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_Perception <-
          acf(
            sample(Data$Response),
            lag.max = length(Data$Response),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_Difficulty <-
          acf(
            sample(Data$Difficulty),
            lag.max = length(Data$Difficulty),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_HardEasy <-
          acf(
            sample(Data$HardEasy),
            lag.max = length(Data$HardEasy),
            na.action = na.pass,
            plot = FALSE
          )
        
        random_acf_External <-
          acf(
            sample(Data$Stimulus),
            lag.max = length(Data$Stimulus),
            na.action = na.pass,
            plot = FALSE
          )
        
        addPerm = data.frame(
          random_acf_Stimulus = random_acf_Stimulus$acf,
          random_acf_History = random_acf_History$acf,
          random_acf_Perception = random_acf_Perception$acf,
          random_acf_RT = random_acf_RT$acf,
          random_acf_Difficulty = random_acf_Difficulty$acf,
          random_acf_HardEasy = random_acf_HardEasy$acf,
          random_acf_External = random_acf_External$acf,
          exceed_acf_Stimulus = as.integer(Data$acf_Stimulus < random_acf_Stimulus$acf),
          exceed_acf_History = as.integer(Data$acf_History < random_acf_History$acf),
          exceed_acf_Perception = as.integer(Data$acf_Perception < random_acf_Perception$acf),
          exceed_acf_RT = as.integer(Data$acf_RT < random_acf_RT$acf),
          exceed_acf_Difficulty = as.integer(Data$acf_Difficulty < random_acf_Difficulty$acf),
          exceed_acf_HardEasy = as.integer(Data$acf_HardEasy < random_acf_HardEasy$acf),
          exceed_acf_External = as.integer(Data$acf_External < random_acf_External$acf)
        )
        
        
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
        mean_random_acf_Perception = mean(random_acf_Perception, na.rm = TRUE),
        mean_random_acf_RT = mean(random_acf_RT, na.rm = TRUE),
        mean_random_acf_Difficulty = mean(random_acf_Difficulty, na.rm = TRUE),
        mean_random_acf_HardEasy = mean(random_acf_HardEasy, na.rm = TRUE),
        mean_random_acf_External = mean(random_acf_External, na.rm = TRUE),
        
        sum_exceed_acf_Stimulus = sum(exceed_acf_Stimulus, na.rm = TRUE),
        sum_exceed_acf_History = sum(exceed_acf_History, na.rm = TRUE),
        sum_exceed_acf_Perception = sum(exceed_acf_Perception, na.rm = TRUE),
        sum_exceed_acf_RT = sum(exceed_acf_RT, na.rm = TRUE),
        sum_exceed_acf_Difficulty = sum(exceed_acf_Difficulty, na.rm = TRUE),
        sum_exceed_acf_HardEasy = sum(exceed_acf_HardEasy, na.rm = TRUE),
        sum_exceed_acf_External = sum(exceed_acf_External, na.rm = TRUE)
      )
      
      Data$random_acf_History = Summ_Permutations$mean_random_acf_History
      Data$random_acf_Stimulus = Summ_Permutations$mean_random_acf_Stimulus
      Data$random_acf_Perception = Summ_Permutations$mean_random_acf_Perception
      Data$random_acf_RT = Summ_Permutations$mean_random_acf_RT
      Data$random_acf_Difficulty = Summ_Permutations$mean_random_acf_Difficulty
      Data$random_acf_HardEasy = Summ_Permutations$mean_random_acf_HardEasy
      Data$random_acf_External = Summ_Permutations$mean_random_acf_External
      
      
      Data$exceed_acf_History = Summ_Permutations$sum_exceed_acf_History
      Data$exceed_acf_Stimulus = Summ_Permutations$sum_exceed_acf_Stimulus
      Data$exceed_acf_Perception = Summ_Permutations$sum_exceed_acf_Perception
      Data$exceed_acf_RT = Summ_Permutations$sum_exceed_acf_RT
      Data$exceed_acf_Difficulty = Summ_Permutations$sum_exceed_acf_Difficulty
      Data$exceed_acf_HardEasy = Summ_Permutations$sum_exceed_acf_HardEasy
      Data$exceed_acf_External = Summ_Permutations$sum_exceed_acf_External
      
      
      
      add_MwData = data.frame(
        subject_id = rep(subject_counter, nrow(Data)),
        session_id = rep(session_counter, nrow(Data)),
        prob_left = Data$trial_stim_prob_left,
        Trial = Data$Trial,
        Stimulus = Data$Stimulus,
        Response = Data$Response,
        Response_minus_1 = Data$Response_minus_1,
        Stimulus_minus_1 = Data$Stimulus_minus_1,
        Accuracy = Data$Accuracy,
        History = Data$History,
        Stimulus_History = Data$Stimulus_History,
        RT = Data$RT,
        Difficulty = Data$Difficulty,
        HardEasy = Data$HardEasy,
        acf_Stimulus = Data$acf_Stimulus,
        acf_History = Data$acf_History,
        acf_Perception = Data$acf_Perception,
        acf_RT = Data$acf_RT,
        acf_Difficulty = Data$acf_Difficulty,
        acf_HardEasy = Data$acf_HardEasy,
        acf_External = Data$acf_External,
        random_acf_Stimulus = Data$random_acf_Stimulus,
        random_acf_History = Data$random_acf_History,
        random_acf_Perception = Data$random_acf_Perception,
        random_acf_RT = Data$random_acf_RT,
        random_acf_Difficulty = Data$random_acf_Difficulty,
        random_acf_HardEasy = Data$random_acf_HardEasy,
        random_acf_External = Data$random_acf_External,
        exceed_acf_Stimulus = Data$exceed_acf_Stimulus,
        exceed_acf_History = Data$exceed_acf_History,
        exceed_acf_Perception = Data$exceed_acf_Perception,
        exceed_acf_RT = Data$exceed_acf_RT,
        exceed_acf_Difficulty = Data$exceed_acf_Difficulty,
        exceed_acf_HardEasy = Data$exceed_acf_HardEasy,
        exceed_acf_External = Data$exceed_acf_External
      )
      MwData <- rbind(MwData, add_MwData)
    }
    
  }
  
  return(MwData)
  
}
