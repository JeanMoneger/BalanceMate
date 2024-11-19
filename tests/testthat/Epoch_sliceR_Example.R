set.seed(123)  # For reproducibility

# Create a data frame with 10 participants, each with 1000 samples
df <- data.frame(
  Part = rep(1:10, each = 1000),  # Participant IDs
  CoPX = rnorm(10000),         # Random measurements for CoP_X
  CoPY = rnorm(10000)          # Random measurements for CoP_Y
)
#ID_col <- "ID"
#columns_to_synthesize <- c("CoP_X", "CoP_Y")
epoch_length <- 1        # Epoch length in seconds
sample_rate <- 100       # Sampling rate in Hz
session_duration <- 10   # Session duration in seconds

epoch_data <- Epoch_SliceR(df, "Part", columns_to_synthesize = "CoPY", epoch_length = 1, sample_rate = 100, session_duration = 10)
library(dplyr)
df_test<- Time_StampeR(df = df, id_col= "Part", sample_rate = 100, protocol_duration = 10)
df_test <-subset(df_test, df_test$Time < 1)
df_test <- df_test %>% group_by(Part) %>% mutate(MeanCop = mean(CoPY, na.rm = T)) %>% distinct(Part, .keep_all=T)

epoch_data <- Epoch_SliceR(
  df = df,
  ID = ID_col,
  columns_to_synthesize = columns_to_synthesize,
  epoch_length = epoch_length,
  sample_rate = sample_rate,
  session_duration = session_duration
)
