## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BalanceMate)
library(knitr)
library(kableExtra)
library(ggplot2)

## ----echo = F, warning = FALSE, message=F-------------------------------------
# Load the required packages

# if (requireNamespace("knitr", quietly = TRUE)) {
#   library(knitr)
# }
# if (requireNamespace("kableExtra", quietly = TRUE)) {
#   library(kableExtra)
# }
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   library(ggplot2)
# }
# Define the data
time_periods <- data.frame(
  Periods = c("Training (20s)", 
              "Fixation Cross (5s)", 
              "Unpleasant trial (15s)", 
              "Fixation Cross (5s)", 
              "Pleasant trial (15s)", 
              "Fixation Cross (5s)", 
              "Unpleasant trial (15s)", 
              "Fixation Cross (5s)", 
              "Pleasant trial (15s)"), 
  Label = c("Training", "Fix", "Unpl_Trial", "Fix", "Pleas_Trial", "Fix", "Unpl_Trial",
            "Fix", "Pleas_Trial"),
  `End Time (s)` = c(20, 
                     25, 
                     40, 
                     45, 
                     60, 
                     65, 
                     80, 
                     85, 
                     100)
)

# Create a neat HTML table
kableExtra::kable(time_periods, format = "html", col.names = c("Periods of interest", "Label", "End Time (s)"), table.attr = "style='width:50%;'") %>%
  kable_styling(
    full_width = FALSE, 
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    position = "center"
  ) 


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


# Identify the time cuts in your protocol: 
cuts<-c(20, 
        25, 
        40, 
        45, 
        60, 
        65, 
        80, 
        85)

# Label the periods:
Labels = c("Training", 
              "Fix", 
              "Unpleasant_t", 
              "Fix", 
              "Pleasant_t", 
              "Fix", 
              "Unpleasant_t", 
              "Fix", 
              "Pleasant_t")

Annotated_Data <- Time_StampeR(df = Data, id_col = "file_name", sample_rate = 100, protocol_duration = 100, cuts = cuts, period_names = Labels)

head(Annotated_Data)

tail(Annotated_Data)


## -----------------------------------------------------------------------------
Crit_Data <- subset(Annotated_Data, Annotated_Data$Period_Name=="Unpleasant_t" | Annotated_Data$Period_Name == "Pleasant_t")

head(Crit_Data)

tail(Crit_Data)

