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

Data<-Merge_PosData(".", SampleRate = 100, SessionDuration = 100)


Epoch_SliceR(df = Data,
      ID = "file_name",
      columns_to_synthesize = c("CoP_X", "CoP_Y"),
      epoch_length = 1,
      sample_rate=100,
      session_duration = 100)[1:20,] # Only display the first 20 rows of the resulting data set

