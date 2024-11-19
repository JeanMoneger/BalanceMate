
Data<- Merge_PosData("ExampleData", SampleRate = 100, SessionDuration = 331)


SpaghettEllipse2animate(df = Data,
                 participant_id_col = "file_name",
                 participant_id = "154931b4.txt",
                 time_col = "Time",
                 time_start = 2,
                 time_end = 33.07,
                 copx_col = "CoP_X",
                 copy_col = "CoP_Y",
                 Animate = T,
                 animation_interval = 0.01,
                 save_animation = T,
                 animation_file = "TestAnimate.gif")
