path_to_data <- system.file("extdata", package = "BalanceMate") #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)
# Stamp ##### Cut every trial (PassView [10s] + GoNoGo [1s] + ITI [2.5s])
# Remove First 7 seconds / Each block started with 7 seconds of blank screen
# Repeat "X" 5 times and "Y" 15 times /// rep(c("ITI", "PassView", "GoNoGo"), times = c(250, 1000, 100))
# # My n row = (147*4)*(700 + (1350*24))
Data2<- Time_StampeR(df = Data,
                     id_col = "file_name",
                     sample_rate = 100,
                     protocol_duration = 331,
                     cuts = c(7,                                       # Blank 7 seconds before each block
                              9.5, 19.5, 20.5,                         # trial 1
                             (9.5+13.5*1 ), (19.5+13.5*1 ), (20.5+13.5*1 ),  # trial 2
                             (9.5+13.5*2 ), (19.5+13.5*2 ), (20.5+13.5*2 ),  # trial 3
                             (9.5+13.5*3 ), (19.5+13.5*3 ), (20.5+13.5*3 ),  # trial 4
                             (9.5+13.5*4 ), (19.5+13.5*4 ), (20.5+13.5*4 ),  # trial 5
                             (9.5+13.5*5 ), (19.5+13.5*5 ), (20.5+13.5*5 ),  # trial 6
                             (9.5+13.5*6 ), (19.5+13.5*6 ), (20.5+13.5*6 ),  # trial 7
                             (9.5+13.5*7 ), (19.5+13.5*7 ), (20.5+13.5*7 ),  # trial 8
                             (9.5+13.5*8 ), (19.5+13.5*8 ), (20.5+13.5*8 ),  # trial 9
                             (9.5+13.5*9 ), (19.5+13.5*9 ), (20.5+13.5*9 ),  # trial 10
                             (9.5+13.5*10), (19.5+13.5*10), (20.5+13.5*10),  # trial 11
                             (9.5+13.5*11), (19.5+13.5*11), (20.5+13.5*11),  # trial 12
                             (9.5+13.5*12), (19.5+13.5*12), (20.5+13.5*12),  # trial 13
                             (9.5+13.5*13), (19.5+13.5*13), (20.5+13.5*13),  # trial 14
                             (9.5+13.5*14), (19.5+13.5*14), (20.5+13.5*14),  # trial 15
                             (9.5+13.5*15), (19.5+13.5*15), (20.5+13.5*15),  # trial 16
                             (9.5+13.5*16), (19.5+13.5*16), (20.5+13.5*16),  # trial 17
                             (9.5+13.5*17), (19.5+13.5*17), (20.5+13.5*17),  # trial 18
                             (9.5+13.5*18), (19.5+13.5*18), (20.5+13.5*18),  # trial 19
                             (9.5+13.5*19), (19.5+13.5*19), (20.5+13.5*19),  # trial 20
                             (9.5+13.5*20), (19.5+13.5*20), (20.5+13.5*20),  # trial 21
                             (9.5+13.5*21), (19.5+13.5*21), (20.5+13.5*21),  # trial 22
                             (9.5+13.5*22), (19.5+13.5*22), (20.5+13.5*22),  # trial 23
                             (9.5+13.5*23), (19.5+13.5*23)),               # trial 24
                     period_names = c("BLANK",                      # Blank 7 seconds before eac
                                      "cross", "trial1", "blank",   # trial 1
                                      "cross", "trial2", "blank",   # trial 2
                                      "cross", "trial3", "blank",   # trial 3
                                      "cross", "trial4", "blank",   # trial 4
                                      "cross", "trial5", "blank",   # trial 5
                                      "cross", "trial6", "blank",   # trial 6
                                      "cross", "trial7", "blank",   # trial 7
                                      "cross", "trial8", "blank",   # trial 8
                                      "cross", "trial9", "blank",   # trial 9
                                      "cross", "trial10", "blank",  # trial 10
                                      "cross", "trial11", "blank",  # trial 11
                                      "cross", "trial12", "blank",  # trial 12
                                      "cross", "trial13", "blank",  # trial 13
                                      "cross", "trial14", "blank",  # trial 14
                                      "cross", "trial15", "blank",  # trial 15
                                      "cross", "trial16", "blank",  # trial 16
                                      "cross", "trial17", "blank",  # trial 17
                                      "cross", "trial18", "blank",  # trial 18
                                      "cross", "trial19", "blank",  # trial 19
                                      "cross", "trial20", "blank",  # trial 20
                                      "cross", "trial21", "blank",  # trial 21
                                      "cross", "trial22", "blank",  # trial 22
                                      "cross", "trial23", "blank",  # trial 23
                                      "cross", "trial24", "blank")  # trial 24
                     )


# Remove unnecessary | OUT PACKAGE

Data<- subset(Data2, Data2$Period_Name != "blank" & Data2$Period_Name != "BLANK" & Data2$Period_Name != "cross"  )

# Plot part 219881b4.txt

#SpaghettEllipse(df = Data, participant_id_col = "file_name", participant_id = "Postural_DataD.txt", time_col = "Time", copx_col = "CoP_X", copy_col = "CoP_Y")

# Merge Second level

# Epoch / I need to create a unique identifier for fileName > Trial number

Data3 <- Epoch_SliceR2(df = Data, ID = c("file_name", "Period_Name"), columns_to_synthesize = c("CoP_X","CoP_Y"), epoch_length = 1, sample_rate = 100, session_duration = 10)
sdCopyMean <- sd(Data3$SD_CoP_Y)
sdCopxMean <- sd(Data3$SD_CoP_X)
#Specific part B4140514
Postural_DataD.txt <- subset(Data3, Data3$file_name == "Postural_DataD.txt")
sdCopyMeanB4140514 <- sd(Postural_DataD.txt$SD_CoP_Y)
sdCopxMeanB4140514 <- sd(Postural_DataD.txt$SD_CoP_X)
# Now I compute Mean CoPs at the second level for each participant | Out PACKAGE

Data_Epoch <- Data3 %>% dplyr::group_by(file_name, Epoch) %>% dplyr::mutate(Mean_CoP_Y2 = mean(Mean_CoP_Y), Mean_CoP_X2 = mean(Mean_CoP_X)) %>% distinct(Mean_CoP_Y2, .keep_all = T)

# Plot2:


# General means

CopyMean <- mean(Data_Epoch$Mean_CoP_Y2)
CopxMean <- mean(Data_Epoch$Mean_CoP_X2)

# Specific part B4140514
Postural_DataD <- subset(Data_Epoch, Data_Epoch$file_name == "Postural_DataD.txt")
CopyMeanPostural_DataD <- mean(Postural_DataD$Mean_CoP_Y2)
CopxMeanPostural_DataD <- mean(Postural_DataD$Mean_CoP_X2)






##### NOW : initial way Wrangling:

library(tidyverse)


ntrial = 24 # There are 96 trials per participant
nSample = 1350 # Each trial length is 13.5s long (ITI = 2.5, Passive Viewing = 10s, Go No GO = 1s); We're working at 100Hz
nblock = 1 # Number of blocks
npart<-6 # Number of participants !!!!!!!! IMPRTANT TO CHANGE IF CHANGES ARE MADE (i.e., excluding participants from analyses before hand - e.g., if some one has not made all blocks)

## Please, correct the Work Directions if necessary (here we want to access the folder "PlatformData"):
#getwd()
#setwd(.../.../...)
setwd(path_to_data)
# Create a file list with all postural data files
file_list <- list.files(pattern = ".*.txt$", recursive = TRUE)

# grouping the files
grouped_files <- split(file_list, sub("/.*", "", file_list))

# Read and stack the content of each file
merged_content <- lapply(names(grouped_files), function(group_name) {
  files <- grouped_files[[group_name]]
  data_frames <- lapply(files, function(file) {
    # Read the file as a data frame
    df <- read.table(file, header = FALSE, sep = ",", stringsAsFactors = FALSE)

    # Add a new column with the file name
    df$file_name <- file
    return(df)
  })
  merged_df <- do.call(rbind, data_frames)
  return(merged_df)
})

# Combine all the data frames into a single data frame
merged_data <- do.call(rbind, merged_content)
colnames(merged_data) <- c("Fx","Fy","Fz","Mx","My", "Mz", "File_name") # Add the column names


# Add time

# With 143 participants, 4 blocks, 7 seconds at each block start, + 13.5s for each of the 24 stim in each block
# My n row = (147*4)*(700 + (1350*24)) = 147*4 * 33100
merged_data$Time <- rep(1:33100, time = npart*nblock)



# STOP! in the naaaame of looove

head(merged_data)
head(Data2)


merged_data$Fx2<-Data2$Fx
merged_data$Test <- merged_data$Fx2-Data2$Fx
max(merged_data$Test)
min(merged_data$Test)



merged_data$time2 <- Data2$Time *100+1
merged_data$copX2 <- Data2$CoP_X
merged_data$copY2 <- Data2$CoP_Y


##### Cut every trial (PassView [10s] + GoNoGo [1s] + ITI [2.5s])

# Remove First 7 seconds / Each block started with 7 seconds of blank screen
merged_data<- subset(merged_data, merged_data$Time >= 701)

# Repeat "X" 5 times and "Y" 15 times
my_vector <- c(rep(c("ITI", "PassView", "GoNoGo"), times = c(250, 1000, 100)))

# Repeat the pattern 20 times
final_vector <- rep(my_vector, times = ntrial) # OK, j'ai un doute là. Est-ce que sur le premier essai il n'y a pas "0.5s - PassView - Go No Go - 2s" A vérifier. Très important. UPDATE: Vérifié, c'est correct.
#final_vector<- c(rep("BLANK", times = 700), final_vector)
merged_data$Phase <- rep(final_vector, times = npart)
Data2b<-subset(Data2, Data2$Period_Name != "BLANK")


merged_data$Phase2 <- Data2b$Period_Name


# Recompute time at the trial level:
#merged_data$time_trial <- rep(rep(seq(0, 13.49, by = 0.01), times = ntrial), times = npart)
merged_data$time_trial <- rep(rep(seq(0.01, 13.50, by = 0.01), times = ntrial), times = npart)


# Every 13.5s, there is a new trial, and this for a total of 24*13.5= 324 s
merged_data$trial_number <- rep(rep(seq(1, ntrial, by = 1), each = nSample), times = npart)

### COP calculations: Check if that's consistent with output from bioanalysis

merged_data$copX <- -merged_data$My / merged_data$Fz
merged_data$copY <- merged_data$Mx / merged_data$Fz # Source: http://silver.neep.wisc.edu/~lakes/BME315L2a.pdf & https://github.com/Jythen/code_descriptors_postural_control/blob/main/main.py



### Stop: in the name of love
merged_data$test7<-merged_data$copX2 -merged_data$copX

# Identicité entre 'merged data' et 'Data2.'

CorrDF <- merged_data %>%
  dplyr::filter(Phase == "PassView")


## STOP! In the naaaaame of loooove


CorrDF$copY2 <- Data$CoP_Y
CorrDF$copX2 <- Data$CoP_X
CorrDF$trial_number2 <- Data$Period_Name

colnames(CorrDF)

CorrDF$Test2 <- CorrDF$copY2-CorrDF$copY
CorrDF$Test3 <- CorrDF$copX2-CorrDF$copX
max(CorrDF$Test2)
min(CorrDF$Test2)
max(CorrDF$Test3)
min(CorrDF$Test3)


# Average by second
CorrDF$Second<- rep(rep(rep(1:10, each =100), times = 24), times = npart)
CorrDF$Continuous_time<- rep(rep(seq(0.01, 10, by = 0.01), times = ntrial), times = npart)


CorrDFEpoch <- CorrDF%>% group_by(File_name, Second) %>% mutate(meanCoPY = mean(copY), meanCoPX = mean(copX)) %>% distinct(Second, .keep_all = T)


CorrDFEpoch$meanCoPX2 <- Data_Epoch$Mean_CoP_X2
CorrDFEpoch$meanCoPY2 <- Data_Epoch$Mean_CoP_Y2

CorrDFEpoch$Newtest <- CorrDFEpoch$meanCoPX2-CorrDFEpoch$meanCoPX
min(CorrDFEpoch$Newtest)
max(CorrDFEpoch$Newtest)

CorrDFEpoch$Newtest <- CorrDFEpoch$meanCoPY2-CorrDFEpoch$meanCoPY
min(CorrDFEpoch$Newtest)
max(CorrDFEpoch$Newtest)




