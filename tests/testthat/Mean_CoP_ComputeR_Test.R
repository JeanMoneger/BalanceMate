path_to_data <- system.file("extdata", package = "BalanceMate") #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)


Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name")
Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 1)
Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 2)
