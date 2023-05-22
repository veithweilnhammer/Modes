f_collect_data <- function(Experiments) {
  
  PwData = data.frame()
  subject_counter = 0
  
  list_for_difficulty =  c("Difficulty", "difficulty", "Difference", "SN", "Dot_diff", "Congruency", "Coherence", "DotProportion", "coh_level", "Contrast", "contrast", "coherence", "Validity", "DotDiff", "contrast_difference", "Setsize", "NoiseLevel", "NoiseLevel_Deg", "Temporal_distance"
  )
  
  ## loop over studies
  for (study_id in c(1:nrow(Experiments))) {
    # for (study_id in c(1:2)) {
    
    print(study_id)
    
    tryCatch(
      expr = {
        Data <-
          read.csv(sprintf(
            "Confidence Database/data_%s.csv",
            Experiments[study_id,]$Name_in_database
          ))
        
        
        
        if ("ï..Subj_idx" %in% colnames(Data))
        {
          Data$Subj_idx <- Data$ï..Subj_idx
        }
        
        
        
        if (sum(colnames(Data) == "Subj_idx") == 0) {
          stop("no subj idx")
        }
        
        
        ## replace matlab syntax
        Data[Data == "NaN"] = NA
        
        ## convert to numeric labels
        Data$Stimulus <- as.numeric(Data$Stimulus)
        Data$Response <- as.numeric(Data$Response)
        
        ## get t-1 history
        Data$Response_minus_1 <- c(NA, Data$Response[1:nrow(Data) - 1])
        
        ## prepare data for binary / continuous responses: Accuracy + History
        Experiments$Type[study_id] <-
          checkBinaryTrait(Data$Response, naVal = "NA")
        if (Experiments$Type[study_id] != "con") {
          Data$History = as.numeric(Data$Response == Data$Response_minus_1)
          Data$Accuracy = as.numeric(Data$Stimulus == Data$Response)
        }
        
        if (Experiments$Type[study_id] == "con") {
          Data$Accuracy <- abs(Data$Stimulus - Data$Response)
          Data$History <- abs(Data$Response - Data$Response_minus_1)
        }
        
        
        RT_columns = which(colnames(Data) == grep('RT', colnames(Data), value =
                                                    TRUE))
        if (sum(RT_columns) > 0) {
          Data$cRT <- scale(as.numeric(Data[, min(RT_columns)]))
        } else {
          Data$cRT <- rep(NA, nrow(Data))
        }
        
        Condition_columns = which(colnames(Data) == grep('Condition', colnames(Data), value =
                                                           TRUE))
        
        if (sum(Condition_columns) > 0) {
          Data$cCondition <- scale(as.numeric(Data[, min(Condition_columns)]))
        } else {
          Data$cCondition <- rep(NA, nrow(Data))
        }
        
        
        Difficulty_columns = which(list_for_difficulty %in% colnames(Data))
        if (sum(Difficulty_columns) > 0) {
          Data$cDifficulty <- scale(as.numeric(Data[, list_for_difficulty[min(Difficulty_columns)]]))
        } else {
          Data$cDifficulty <- rep(NA, nrow(Data))
        }
        
        Data$Confidence <- scale(as.numeric(Data$Confidence))
        
        ## loop over participants
        for (id in unique(Data$Subj_idx)) {
          index = Data$Subj_idx == id
          
          # get trial numbers
          Data$Trial[index] = 1:nrow(Data[index, ])
          
          # get autocorrelations
          acf_Stimulus <-
            acf(
              Data[index,]$Accuracy,
              lag.max = length(Data[index,]$Accuracy),
              na.action = na.pass,
              plot = FALSE
            )
          
          acf_History <-
            acf(
              Data[index,]$History,
              lag.max = length(Data[index,]$History),
              na.action = na.pass,
              plot = FALSE
            )
          
          
          acf_Perception <-
            acf(
              Data[index,]$Response,
              lag.max = length(Data[index,]$Response),
              na.action = na.pass,
              plot = FALSE
            )
          
          acf_Confidence <-
            acf(
              Data[index,]$Confidence,
              lag.max = length(Data[index,]$Confidence),
              na.action = na.pass,
              plot = FALSE
            )
          
          Data$acf_History[index] = acf_History$acf
          Data$acf_Stimulus[index] = acf_Stimulus$acf
          Data$acf_Perception[index] = acf_Perception$acf
          Data$acf_Confidence[index] = acf_Confidence$acf
          
          if (sum(!is.na(Data[index,]$cRT)) > 0) {
            acf_RT <-
              acf(
                Data[index,]$cRT,
                lag.max = length(Data[index,]$cRT),
                na.action = na.pass,
                plot = FALSE
              )
            
            Data$acf_RT[index] = acf_RT$acf
          } else {
            Data$acf_RT[index] <- rep(NA, length(Data[index,]$cRT))
          }
          
          if (sum(!is.na(Data[index,]$cDifficulty)) > 0) {
            acf_Difficulty <-
              acf(
                Data[index,]$cDifficulty,
                lag.max = length(Data[index,]$cDifficulty),
                na.action = na.pass,
                plot = FALSE
              )
            
            Data$acf_Difficulty[index] = acf_Difficulty$acf
          } else {
            Data$acf_Difficulty[index] <- rep(NA, length(Data[index,]$cDifficulty))
          }
          
          if (sum(!is.na(Data[index,]$cCondition)) > 0) {
            acf_Condition <-
              acf(
                Data[index,]$cCondition,
                lag.max = length(Data[index,]$cCondition),
                na.action = na.pass,
                plot = FALSE
              )
            
            Data$acf_Condition[index] = acf_Condition$acf
          } else {
            Data$acf_Condition[index] <- rep(NA, length(Data[index,]$cCondition))
          }
          
          
          
          Permutations = data.frame()
          for (perm_idx in c(1:n_permutations)){
            addPerm = data.frame()  
            
            random_acf_Stimulus <-
              acf(
                sample(Data[index,]$Accuracy),
                lag.max = length(Data[index,]$Accuracy),
                na.action = na.pass,
                plot = FALSE
              )
            
            random_acf_History <-
              acf(
                sample(Data[index,]$History),
                lag.max = length(Data[index,]$History),
                na.action = na.pass,
                plot = FALSE
              )
            
            random_acf_Perception <-
              acf(
                sample(Data[index,]$Response),
                lag.max = length(Data[index,]$Response),
                na.action = na.pass,
                plot = FALSE
              )
            
            random_acf_Confidence <-
              acf(
                sample(Data[index,]$Confidence),
                lag.max = length(Data[index,]$Confidence),
                na.action = na.pass,
                plot = FALSE
              )
            
            addPerm = data.frame(
              random_acf_Stimulus = random_acf_Stimulus$acf,
              random_acf_History = random_acf_History$acf,
              random_acf_Perception = random_acf_Perception$acf,
              random_acf_Confidence = random_acf_Confidence$acf,
              exceed_acf_Stimulus = as.integer(Data$acf_Stimulus[index] < random_acf_Stimulus$acf),
              exceed_acf_History = as.integer(Data$acf_History[index] < random_acf_History$acf),
              exceed_acf_Perception = as.integer(Data$acf_Perception[index] < random_acf_Perception$acf),
              exceed_acf_Confidence = as.integer(Data$acf_Confidence[index] < random_acf_Confidence$acf)
            )
            
            
            if (sum(!is.na(Data[index,]$cRT)) > 0) {
              
              random_acf_RT <-
                acf(
                  sample(Data[index,]$cRT),
                  lag.max = length(Data[index,]$cRT),
                  na.action = na.pass,
                  plot = FALSE
                )
              
              addPerm$random_acf_RT = random_acf_RT$acf 
              addPerm$exceed_acf_RT = as.integer(Data$acf_RT[index] < random_acf_RT$acf)
            } else {
              addPerm$random_acf_RT = rep(NA, length(random_acf_Stimulus$acf))
              addPerm$exceed_acf_RT = rep(NA, length(random_acf_Stimulus$acf))
            }
            
            if (sum(!is.na(Data[index,]$cDifficulty)) > 0) {
              
              random_acf_Difficulty <-
                acf(
                  sample(Data[index,]$cDifficulty),
                  lag.max = length(Data[index,]$cDifficulty),
                  na.action = na.pass,
                  plot = FALSE
                )
              
              addPerm$random_acf_Difficulty = random_acf_Difficulty$acf 
              addPerm$exceed_acf_Difficulty = as.integer(Data$acf_Difficulty[index] < random_acf_Difficulty$acf)
            } else {
              addPerm$random_acf_Difficulty = rep(NA, length(random_acf_Stimulus$acf))
              addPerm$exceed_acf_Difficulty = rep(NA, length(random_acf_Stimulus$acf))
            }
            
            if (sum(!is.na(Data[index,]$cCondition)) > 0) {
              
              random_acf_Condition <-
                acf(
                  sample(Data[index,]$cCondition),
                  lag.max = length(Data[index,]$cCondition),
                  na.action = na.pass,
                  plot = FALSE
                )
              
              addPerm$random_acf_Condition = random_acf_Condition$acf 
              addPerm$exceed_acf_Condition = as.integer(Data$acf_Condition[index] < random_acf_Condition$acf)
            } else {
              addPerm$random_acf_Condition = rep(NA, length(random_acf_Stimulus$acf))
              addPerm$exceed_acf_Condition = rep(NA, length(random_acf_Stimulus$acf))
            }
            
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
            mean_random_acf_Confidence = mean(random_acf_Confidence, na.rm = TRUE),
            mean_random_acf_RT = mean(random_acf_RT, na.rm = TRUE),
            mean_random_acf_Difficulty = mean(random_acf_Difficulty, na.rm = TRUE),
            mean_random_acf_Condition = mean(random_acf_Condition, na.rm = TRUE),
            
            sum_exceed_acf_Stimulus = sum(exceed_acf_Stimulus, na.rm = TRUE),
            sum_exceed_acf_History = sum(exceed_acf_History, na.rm = TRUE),
            sum_exceed_acf_Perception = sum(exceed_acf_Perception, na.rm = TRUE),
            sum_exceed_acf_Confidence = sum(exceed_acf_Confidence, na.rm = TRUE),
            sum_exceed_acf_RT = sum(exceed_acf_RT, na.rm = TRUE),
            sum_exceed_acf_Difficulty = sum(exceed_acf_Difficulty, na.rm = TRUE),
            sum_exceed_acf_Condition = sum(exceed_acf_Condition, na.rm = TRUE)
          )
          
          Data$random_acf_History[index] = Summ_Permutations$mean_random_acf_History
          Data$random_acf_Stimulus[index] = Summ_Permutations$mean_random_acf_Stimulus
          Data$random_acf_Perception[index] = Summ_Permutations$mean_random_acf_Perception
          Data$random_acf_Confidence[index] = Summ_Permutations$mean_random_acf_Confidence
          Data$random_acf_RT[index] = Summ_Permutations$mean_random_acf_RT
          Data$random_acf_Difficulty[index] = Summ_Permutations$mean_random_acf_Difficulty
          Data$random_acf_Condition[index] = Summ_Permutations$mean_random_acf_Condition
          
          
          Data$exceed_acf_History[index] = Summ_Permutations$sum_exceed_acf_History
          Data$exceed_acf_Stimulus[index] = Summ_Permutations$sum_exceed_acf_Stimulus
          Data$exceed_acf_Perception[index] = Summ_Permutations$sum_exceed_acf_Perception
          Data$exceed_acf_Confidence[index] = Summ_Permutations$sum_exceed_acf_Confidence
          Data$exceed_acf_RT[index] = Summ_Permutations$sum_exceed_acf_RT
          Data$exceed_acf_Difficulty[index] = Summ_Permutations$sum_exceed_acf_Difficulty
          Data$exceed_acf_Condition[index] = Summ_Permutations$sum_exceed_acf_Condition
          
          
          ## collect participant data
          subject_counter = subject_counter + 1
          add_PwData = data.frame(
            study_id = rep(study_id, sum(index)),
            study_type = rep(Experiments$Category[study_id], sum(index)),
            response_type = rep(
              checkBinaryTrait(Data$Response, naVal = "NA"),
              sum(index)
            ),
            subject_id = rep(subject_counter, sum(index)),
            Trial = Data$Trial[index],
            Stimulus = Data$Stimulus[index],
            Response = Data$Response[index],
            Response_minus_1 = Data$Response_minus_1[index],
            Accuracy = Data$Accuracy[index],
            History = Data$History[index],
            Confidence = Data$Confidence[index],
            RT = Data$cRT[index],
            Difficulty = Data$cDifficulty[index],
            Condition = Data$cCondition[index],
            acf_Stimulus = Data$acf_Stimulus[index],
            acf_History = Data$acf_History[index],
            acf_Perception = Data$acf_Perception[index],
            acf_Confidence = Data$acf_Confidence[index],
            acf_RT = Data$acf_RT[index],
            acf_Difficulty = Data$acf_Difficulty[index],
            acf_Condition = Data$acf_Condition[index],
            random_acf_Stimulus = Data$random_acf_Stimulus[index],
            random_acf_History = Data$random_acf_History[index],
            random_acf_Perception = Data$random_acf_Perception[index],
            random_acf_Confidence = Data$random_acf_Confidence[index],
            random_acf_RT = Data$random_acf_RT[index],
            random_acf_Difficulty = Data$random_acf_Difficulty[index],
            random_acf_Condition = Data$random_acf_Condition[index],
            exceed_acf_Stimulus = Data$exceed_acf_Stimulus[index],
            exceed_acf_History = Data$exceed_acf_History[index],
            exceed_acf_Perception = Data$exceed_acf_Perception[index],
            exceed_acf_Confidence = Data$exceed_acf_Confidence[index],
            exceed_acf_RT = Data$exceed_acf_RT[index],
            exceed_acf_Difficulty = Data$exceed_acf_Difficulty[index],
            exceed_acf_Condition = Data$exceed_acf_Condition[index]
          )
          PwData <- rbind(PwData, add_PwData)
        }
        
        
      },
      error = function(e){print("error")}
    )
  }
  
  return(PwData)
}