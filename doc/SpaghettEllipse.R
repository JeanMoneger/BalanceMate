## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BalanceMate)

## -----------------------------------------------------------------------------
#path_to_data <- system.file("extdata", package = "BalanceMate")
#Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 100)

# If required: convert Rdata to text files.
setwd("/Users/sylviemoneger/Desktop/BalanceMate/data")
files <- list(
    "Postural_DataA" = "~/Desktop/BalanceMate/data/Postural_DataA.Rdata",
    "Postural_DataB" = "~/Desktop/BalanceMate/data/Postural_DataB.Rdata",
    "Postural_DataC" = "~/Desktop/BalanceMate/data/Postural_DataC.Rdata",
    "Postural_DataD" = "~/Desktop/BalanceMate/data/Postural_DataD.Rdata",
    "Postural_DataE" = "~/Desktop/BalanceMate/data/Postural_DataE.Rdata",
    "Postural_DataF" = "~/Desktop/BalanceMate/data/Postural_DataF.Rdata"
)

# Loop through each file, add a blank row with spaces, and save as .txt
for (name in names(files)) {
    # Load the .Rdata file
    load(files[[name]])
    
    # Dynamically get the object loaded (assuming it's named the same as the file)
    data <- get(name)
    
    # Write to a .txt file with the same name
    write.table(data, file = paste0(name, ".txt"), sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

path_to_data <- system.file("data", package = "BalanceMate")


SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataA.txt", Title = "Participant A")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataB.txt", Title = "Participant B")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataC.txt", Title = "Participant C")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataD.txt", Title = "Participant D")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataE.txt", Title = "Participant E")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataF.txt", Title = "Participant F")

## -----------------------------------------------------------------------------
path_to_data <- system.file("extdata", package = "BalanceMate")
Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 100)

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataA.txt", time_col = "Time", time_start = 10, time_end = 90, Title = "Participant A")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataB.txt",  time_col = "Time", time_start = 10, time_end = 90,Title = "Participant B")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataC.txt",  time_col = "Time", time_start = 10, time_end = 90,Title = "Participant C")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataD.txt",  time_col = "Time", time_start = 10, time_end = 90,Title = "Participant D")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataE.txt",  time_col = "Time", time_start = 10, time_end = 90,Title = "Participant E")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataF.txt",  time_col = "Time", time_start = 10, time_end = 90,Title = "Participant F")

