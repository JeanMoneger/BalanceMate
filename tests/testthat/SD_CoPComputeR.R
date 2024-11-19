Data<- Merge_PosData("ExampleData", SampleRate = 100, SessionDuration = 331)

TestFun<-SD_CoPComputeR(Data, columns_to_synthesize = c("CoP_Y", "CoP_X"),Epoch = 1, Time = "Time", ID = "file_name")


