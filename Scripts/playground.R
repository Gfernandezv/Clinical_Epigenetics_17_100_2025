sorted_data <- out.roi$analisis$D1_8515cropped

  filtered_subset <- data.frame()
  
  # Create a list to store the sequences
  sequence_list <- list(c("H0"),
                        c("H19", "H0"),
                        c("H1", "H0"),
                        c("H18", "H19", "H0"),
                        c("H2", "H1", "H0"),
                        c("H17", "H18", "H19", "H0"),
                        c("H3", "H2", "H1", "H0"))
##
  strategy <- function(data, sequences) {
    filtered_subset <- data[data$SHAPE == "H0", ][1, ]
    filtered_subset$error <- which(data$SHAPE == "H0")[1]
    
    #split_columns <- strsplit(data$VIDEO, "_")
    #filtered_subset$Day <- split_columns[[1]][1]
    #filtered_subset$ID <- split_columns[[1]][2]
    
    sequence_counts <- sapply(sequences, function(seq) {
      str_count(paste(data$SHAPE, collapse = ""), paste(seq, collapse = ""))
    })
    
    if (sequence_counts[1] == 0) {
      filtered_subset$Strategy <- "none"
    } else {
      if ((sequence_counts[2] == 1 || sequence_counts[3] == 1) && filtered_subset$error < 2) {
        filtered_subset$Strategy <- "Direct"
      } else {
        if (any(sequence_counts[4:7] == 1)) {
          filtered_subset$Strategy <- "Serial" 
        } else {
          filtered_subset$Strategy <- "Mixed"
        }
      }
    }
    
    filtered_subset$Latency <- (filtered_subset$`ENTRY FRAMES`)/ 60
    
    return(filtered_subset)
  }
  
  
  analyze_strategy <- strategy2(sorted_data, sequence_list)
   print(analyze_strategy)

   ###################################
   ####
   
   