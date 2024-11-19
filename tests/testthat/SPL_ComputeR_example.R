path_to_data <- system.file("extdata", package = "BalanceMate") #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)


SPL_ComputeR(data = Data, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "file_name", time_col = "Time", epoch_length = 28)
SPL_ComputeR(data = Data, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "file_name")

group_info[,1]
