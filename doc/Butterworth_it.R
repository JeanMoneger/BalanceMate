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


Filter_types_Data <- structure(list(`Filter type` = c(NA, "2nd Order Low-Pass Butterworth Filter (5Hz)", 
"Low Pass filtered (8Hz)", "2nd Order Butterworth (5Hz)", "low pass filtered (5Hz)", 
NA, "5-point moving average", "2nd Order Butterworth Filter (10Hz)", 
NA, NA, NA, NA, NA, "Unknown", "2nd order Butterworth (5Hz cutoff)", 
NA, "second-order low-pass Butterworth filter with a cutoff frequency of 10Ã\u008a Hz", 
"2nd Order Butterworth (10Hz)", "2nd Order Low-Pass Butterworth Filter (10Hz)", 
"15Hz Lowpass filter", "2nd Order Butterworth (5Hz)", "Custom Low-Pass filter", 
"None", NA, NA, "Unknown", NA, NA, "low pass filtered (10Hz)", 
"3rd order butterwoth (0.01 - 10Hz)", NA, "low-pass Butterworth filter (cut-off frequency: 5 Hz", 
"None", NA, NA, "second-order low-pass Butterworth filter with a cutoff frequency of 20Ã\u008a Hz", 
"None", "low pass filtered (5Hz)", NA, NA, NA, NA, "2nd order butterworth low pass filter 5Hz", 
NA), Filter_cutOff = c("No filter used", "5Hz", "8Hz", "5Hz", 
"5Hz", "No filter used", "Other", "10Hz", "No filter used", "No filter used", 
"No filter used", "No filter used", "No filter used", "No filter used", 
"5Hz", "No filter used", "10Hz", "10Hz", "10Hz", "15Hz", "5Hz", 
"Not enough information", "No filter used", "No filter used", 
"No filter used", "No filter used", "No filter used", "No filter used", 
"10Hz", "Not enough information", "No filter used", "5Hz", "No filter used", 
"No filter used", "No filter used", "20Hz", "No filter used", 
"5Hz", "No filter used", "No filter used", "No filter used", 
"No filter used", "5Hz", "No filter used"), Filter_Order = c("N/A", 
"2nd Order", "Unknown", "2nd Order", "Unknown", "N/A", "N/A", 
"2nd Order", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "2nd Order", 
"N/A", "2nd Order", "2nd Order", "2nd Order", "Unknown", "2nd Order", 
"Unknown", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "Unknown", 
"3rd Order", "N/A", "Unknown", "N/A", "N/A", "N/A", "2nd Order", 
"N/A", "Unknown", "N/A", "N/A", "N/A", "N/A", "2nd Order", "N/A"
)), class = "data.frame", row.names = c(NA, -44L))

table1<- table(Filter_types_Data$Filter_cutOff)

# Create a neat HTML table
kableExtra::kable(table1, format = "html", col.names = c("Cut-off Frequency used", "Number of papers"), table.attr = "style='width:50%;'", caption = "Cut-Off Frequency usage in the literature")

table2<- table(Filter_types_Data$Filter_Order)

# Create a neat HTML table
kableExtra::kable(table2, format = "html", col.names = c("Order of the filter used", "Number of papers"), table.attr = "style='width:50%;'" , 
  caption = "Filter's order usage in the literature")


## -----------------------------------------------------------------------------
#Data<-Merge_PosData("~/Desktop/BalanceMate/inst/extdata/", SampleRate = 100, SessionDuration = 100)

# If required: convert Rdata to txt files:
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

filtered_data <- Butterworth_it(
  Data = Data,
  cutoff_freq = 5,
  filter_order = 2,
  sampling_rate = 100,
  type = "low",
  Colname = c("CoP_X", "CoP_Y")
)


## ----fig.show='hold'----------------------------------------------------------
# File names
file_names <- c("Postural_DataB.txt", "Postural_DataA.txt", "Postural_DataD.txt")

# Loop through file names to generate plots side by side
for (file in file_names) {
  # Original signal plot
  plot(
    x = subset(filtered_data, filtered_data$file_name == file & filtered_data$Time > 30 & filtered_data$Time < 32)$Time,
    y = subset(filtered_data, filtered_data$file_name == file & filtered_data$Time > 30 & filtered_data$Time < 32)$CoP_Y,
    type = "l",
    main = paste("Original Signal:", file),
    xlab = "Time",
    ylab = "CoP-Y"
  )
  
  # Filtered signal plot
  plot(
    x = subset(filtered_data, filtered_data$file_name == file & filtered_data$Time > 30 & filtered_data$Time < 32)$Time,
    y = subset(filtered_data, filtered_data$file_name == file & filtered_data$Time > 30 & filtered_data$Time < 32)$CoP_Y_filtered,
    type = "l",
    main = paste("Filtered Signal:", file),
    xlab = "Time",
    ylab = "CoP-Y"
  )
}


