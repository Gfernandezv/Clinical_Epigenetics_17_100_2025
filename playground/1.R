library(readr)
library(stringr)
library(data.table)
library(dplyr)
# Put the csv filename (extension included)
reading_file <- "Data/Detailed_ROI_data_20230911052117.csv"
Working_dir <- "C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis"

# Extract the file name from the path
csv_file_name <- gsub("\\.csv$", "", basename(file.path(Working_dir, reading_file))) 

setwd(Working_dir)
raw.data.ROI <- read_csv(reading_file)


#View(raw.data.ROI)
# Define los FPS
FPS <- 15

# secuencia de busqueda de ROI
sequence_list <- list(c("H0"),
                      c("H19", "H0"),
                      c("H1", "H0"),
                      c("H18", "H19", "H0"),
                      c("H2", "H1", "H0"),
                      c("H17", "H18", "H19", "H0"),
                      c("H3", "H2", "H1", "H0"))


# separar el output de ROI de simba por los videos y días.
unique_vector <- unique(raw.data.ROI$VIDEO)

# Create an empty dataframe to store the filtered rows y una lista para out.roi, la lista hace mas fácil guardar y acceder a los datos
filtered_df <- data.frame()
filtered_subset <- data.frame()
out.roi <- list()


for (i in seq_along(unique_vector)) {
  out.roi$data[[unique_vector[i]]] <- subset(raw.data.ROI, VIDEO == unique_vector[i])
  
  # Order it by entry frames
  out.roi$data[[unique_vector[i]]] <- out.roi$data[[unique_vector[i]]][order(out.roi$data[[unique_vector[i]]]$`ENTRY FRAMES`), ]
  out.roi$data[[unique_vector[i]]]$FIR <- (out.roi$data[[unique_vector[i]]]$`EXIT FRAMES`) - (out.roi$data[[unique_vector[i]]]$`ENTRY FRAMES`)

out.roi$analisis[[unique_vector[i]]] <- FUN_update_and_aggregate_fir(out.roi$data[[unique_vector[i]]])

out.roi$analisis2[[unique_vector[i]]] <- FUN_strategy(out.roi$analisis[[unique_vector[i]]], sequence_list, unique_vector[i])
}

# Create an empty list to store individual data frames
data_frames_list <- list()

# Loop through each element in out.roi$analisis2
for (i in seq_along(out.roi$analisis2)) {
  # Add the data frame from out.roi$analisis2[[i]] to the list
  data_frames_list[[i]] <- out.roi$analisis2[[i]]
}

# Use bind_rows to collapse all data frames into one
collapsed_data <- dplyr::bind_rows(data_frames_list)

# Define the path to save the CSV file
output_file_path <- file.path("Data", paste(csv_file_name, "_analized.csv", sep = ""))

# Write the dataframe to the CSV file
write.csv2(collapsed_data, file = output_file_path)

cat("Analisis listo, se generó el archivo:", output_file_path, "\n")


# Funciones ------------------------------------------------------

# detector de snouts, es capaz de sumar los FIR y separarlos en caso de que sean menores o mayores que determinado umbral.

FUN_update_and_aggregate_fir <- function(sorted_data) {
  # Initialize variables with "UpAgFX" prefix
  updated_fir_vector <- numeric(length(sorted_data$FIR))
  current_shape <- sorted_data$SHAPE[1]
  accumulated_fir <- sorted_data$FIR[1]
  
  # Create vectors to store updated values
  updated_shapes <- character(0)
  updated_firs <- numeric(0)
  enterf <- numeric(0)
  exitf <- numeric(0)
  
  # Initialize variables to track first entry and last exit
  first_entry <- sorted_data$`ENTRY FRAMES`[1]
  last_exit <- sorted_data$`EXIT FRAMES`[1]
  
  # Iterate through the data
  for (i in 2:length(sorted_data$SHAPE)) {
    if (!is.na(sorted_data$`ENTRY FRAMES`[i + 1] - sorted_data$`EXIT FRAMES`[i])) {
      if (sorted_data$SHAPE[i] == sorted_data$SHAPE[i - 1]) {
        accumulated_fir <- accumulated_fir + sorted_data$FIR[i]
        
      } else {
        updated_shapes <- c(updated_shapes, current_shape)
        updated_firs <- c(updated_firs, accumulated_fir)
        enterf <- c(enterf, first_entry)  # Store the first entry frame
        exitf <- c(exitf, last_exit)      # Store the last exit frame
        
        current_shape <- sorted_data$SHAPE[i]
        accumulated_fir <- sorted_data$FIR[i]
        first_entry <- sorted_data$`ENTRY FRAMES`[i]  # Reset first entry frame
      }
    } else if (sorted_data$SHAPE[i] == current_shape) {
      # si no hay cambio en el ROI, pero la diferencia es superior a 3 veces el FPS, se considera como otra visita.
      
      if (i + 1 <= length(sorted_data$SHAPE) && sorted_data$`EXIT FRAMES`[i] - sorted_data$`ENTRY FRAMES`[i + 1] > (3*FPS)) {
        accumulated_fir <- sorted_data$FIR[i]
        #print("mayor")
        
      } else {
        accumulated_fir <- accumulated_fir + sorted_data$FIR[i]
        #print("menor")
      }
    } else {
      updated_shapes <- c(updated_shapes, current_shape)
      updated_firs <- c(updated_firs, accumulated_fir)
      enterf <- c(enterf, first_entry)  # Store the first entry frame
      exitf <- c(exitf, last_exit)      # Store the last exit frame
      
      current_shape <- sorted_data$SHAPE[i]
      accumulated_fir <- sorted_data$FIR[i]
      first_entry <- sorted_data$`ENTRY FRAMES`[i]  # Reset first entry frame
    }
    
    # Update last exit frame
    last_exit <- sorted_data$`EXIT FRAMES`[i]
  }
  
  # Add the last values
  updated_shapes <- c(updated_shapes, current_shape)
  updated_firs <- c(updated_firs, accumulated_fir)
  enterf <- c(enterf, first_entry)  # Store the first entry frame
  exitf <- c(exitf, last_exit)      # Store the last exit frame
  
  # Create the updated_data data frame
  updated_data <- data.frame(SHAPE = updated_shapes, FIR = updated_firs, ROI_Entry = enterf, ROI_Exit = exitf)
  
  return(updated_data)
}

# Example usage:
updated_data <- FUN_update_and_aggregate_fir(sorted_data)
print(updated_data)

 
 ##

 FUN_strategy <- function(data, sequences, data_name) {
   filtered_subset <- data[data$SHAPE == "H0", ][1, ]
   filtered_subset$error <- which(data$SHAPE == "H0")[1]
   
   # Count occurrences of "H0" shapes in the original input data
   h0_count <- sum(data$SHAPE == "H0")
   filtered_subset$H0_Count <- h0_count
   
   #print(data_name)
   split_columns <- strsplit(data_name, "_")
   filtered_subset$Day <- split_columns[[1]][1]
   filtered_subset$ID <- split_columns[[1]][2]
   
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
   
   filtered_subset$Latency <- (data$ROI_Entry[which(data$SHAPE == "H0")[1]])/ FPS
   
   filtered_subset <- filtered_subset %>%
     select(ID, Day, everything())
   
   return(filtered_subset)
 }
 analyze_strategy <- FUN_strategy(sorted_data, sequence_list)
 print(analyze_strategy) 
 