## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BalanceMate)
library(ggplot2)


## ----echo = F, message= F, warning= F-----------------------------------------

# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   library(ggplot2)
# }


outcome_df<-structure(list(Outcome = structure(1:8, levels = c("Amplitude COP", 
"Area Body Sw", "COP Y", "ErrorCoP", "MeanSpeed", "Other", "SD COP", 
"Sway Path Length"), class = "factor"), Frequency = c(1L, 3L, 
20L, 1L, 1L, 1L, 10L, 7L)), class = "data.frame", row.names = c(NA, 
-8L))

ggplot2::ggplot(outcome_df, aes(x = Outcome, y = Frequency, fill = Outcome)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Outcomes",
    x = "Outcome Categories",
    y = "Frequency",
    caption = "Source: Monéger et al., 2024"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set3")  # You can change the palette as needed


## -----------------------------------------------------------------------------
# Data<-Merge_PosData("~/Desktop/BalanceMate/inst/extdata/", SampleRate = 100, SessionDuration = 100)


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
    
    # Add a "blank" row (spaces for all cells) at the top
    #blank_row <- rep(" ", 6)  # Create a row of spaces with the same number of columns
    #data <- rbind(blank_row, data)
    
    # Write to a .txt file with the same name
    write.table(data, file = paste0(name, ".txt"), sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

Data<-Merge_PosData(".", SampleRate = 100, SessionDuration = 100)

# Identify the time cuts in your protocol: 
cuts<-c(20, 
                     22, 
                     30, 
                     32, 
                     40, 
                     42, 
                     50, 
                     52, 
                     60, 
                     62, 
                     70, 
                     72, 
                     80, 
                     82,
                     90, 
                     92)

# Label the periods:
Label = c("Training", 
          "Fix", 
          "Trial_1", 
          "Fix", 
          "Trial_2", 
          "Fix", 
          "Trial_3",
          "Fix", 
          "Trial_4", 
          "Fix", 
          "Trial_5", 
          "Fix", 
          "Trial_6", 
          "Fix", 
          "Trial_7", 
          "Fix", 
          "Trial_8") 


Annotated_Data <- Time_StampeR(df = Data, id_col = "file_name", sample_rate = 100, protocol_duration = 100, cuts = cuts, period_names = Label)

Data <- subset(Annotated_Data, Annotated_Data$Period_Name != "Blank" & Annotated_Data$Period_Name != "Fix")


## -----------------------------------------------------------------------------
CompleteData <- compute_postural_indicators(Data, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "file_name", indicators = c("CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", "SD_CoP_Y"))

head(CompleteData)

## -----------------------------------------------------------------------------
CompleteData <- compute_postural_indicators(Data, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "file_name", time_col = "Time", epoch_length = 1, indicators = c("CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", "SD_CoP_Y"))

head(CompleteData)

