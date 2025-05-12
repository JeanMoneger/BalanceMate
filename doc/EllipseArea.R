## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BalanceMate)

## -----------------------------------------------------------------------------
#path_to_data <- system.file("extdata", package = "BalanceMate")
#' # Input correct arguments: here, the protocol is 100seconds long, the sample rate is 100Hz
#Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 100)

# If required: convert RData to text files.

files <- list(
    "Postural_DataA" = "~/Desktop/BalanceMate/data/Postural_DataA.RData",
    "Postural_DataB" = "~/Desktop/BalanceMate/data/Postural_DataB.RData",
    "Postural_DataC" = "~/Desktop/BalanceMate/data/Postural_DataC.RData",
    "Postural_DataD" = "~/Desktop/BalanceMate/data/Postural_DataD.RData",
    "Postural_DataE" = "~/Desktop/BalanceMate/data/Postural_DataE.RData",
    "Postural_DataF" = "~/Desktop/BalanceMate/data/Postural_DataF.RData"
)

# Loop through each file, add a blank row with spaces, and save as .txt
for (name in names(files)) {
    # Load the .RData file
    load(files[[name]])

    # Dynamically get the object loaded (assuming it's named the same as the file)
    data <- get(name)

    # Write to a .txt file with the same name
    write.table(data, file = paste0(name, ".txt"), sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

Data<-Merge_PosData(".", SampleRate = 100, SessionDuration = 100)



SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataA.txt")


## -----------------------------------------------------------------------------
#Data<-Merge_PosData("~/Desktop/BalanceMate/inst/extdata/", SampleRate = 100, SessionDuration = 100)

files <- list(
    "Postural_DataA" = "~/Desktop/BalanceMate/data/Postural_DataA.RData",
    "Postural_DataB" = "~/Desktop/BalanceMate/data/Postural_DataB.RData",
    "Postural_DataC" = "~/Desktop/BalanceMate/data/Postural_DataC.RData",
    "Postural_DataD" = "~/Desktop/BalanceMate/data/Postural_DataD.RData",
    "Postural_DataE" = "~/Desktop/BalanceMate/data/Postural_DataE.RData",
    "Postural_DataF" = "~/Desktop/BalanceMate/data/Postural_DataF.RData"
)

# Loop through each file, add a blank row with spaces, and save as .txt
for (name in names(files)) {
    # Load the .RData file
    load(files[[name]])

    # Dynamically get the object loaded (assuming it's named the same as the file)
    data <- get(name)

    # Write to a .txt file with the same name
    write.table(data, file = paste0(name, ".txt"), sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

Data<-Merge_PosData(".", SampleRate = 100, SessionDuration = 100)




## -----------------------------------------------------------------------------
BalanceMate::compute_ellipse_area(data = Data,
                                  CoPX_col = "CoP_X",
                                  CoPY_col = "CoP_Y",
                                  ID = "file_name"
                                  )

## -----------------------------------------------------------------------------
SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataA.txt", Title = "Participant A")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataB.txt", Title = "Participant B")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataC.txt", Title = "Participant C")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataD.txt", Title = "Participant D")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataE.txt", Title = "Participant E")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataF.txt", Title = "Participant F")


## -----------------------------------------------------------------------------
BalanceMate::compute_ellipse_area(data = Data,
                                  CoPX_col = "CoP_X",
                                  CoPY_col = "CoP_Y",
                                  ID = "file_name",
                                  time_col = "Time",
                                  epoch_length = 25)

## -----------------------------------------------------------------------------
BalanceMate::compute_ellipse_area(data = Data,
                                  CoPX_col = "CoP_X",
                                  CoPY_col = "CoP_Y",
                                  ID = "file_name",
                                  confint = .8)

## -----------------------------------------------------------------------------
SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataA.txt",conf_level = .8, Title = "Participant A")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataB.txt", conf_level = .8,Title = "Participant B")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataC.txt",conf_level = .8, Title = "Participant C")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataD.txt",conf_level = .8, Title = "Participant D")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataE.txt",conf_level = .8, Title = "Participant E")

SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataF.txt",conf_level = .8, Title = "Participant F")


