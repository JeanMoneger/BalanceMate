# Sample data with variable rows per participant
set.seed(123)
df <- data.frame(
  "ID" = rep(1:3, times = c(950, 1000, 1050)),  # Participants with 950, 1000, and 1050 rows
  "Mx" = rnorm(950 + 1000 + 1050)
)

# Apply the function without periods
df_with_time <- Time_StampeR(df, sample_rate = 100, protocol_duration = 10)

# View the warnings and data
# Warnings about participants with row counts different from expected will be displayed
head(df_with_time)




# Define cuts and period names
cuts <- c(2, 9)
period_names <- c("Training", "Trial", "Blank")

# Apply the function with periods
df_with_periods <- Time_StampeR(df, sample_rate = 100, protocol_duration = 10, cuts = cuts, period_names = period_names)

# View the warnings and data
# Warnings about participants with row counts different from expected will be displayed
head(df_with_periods)

####

# Participant with only 800 rows (missing data)
df_incomplete <- data.frame(
  "ID" = rep(1, 800),
  "Mx" = rnorm(800)
)

# Apply the function
df_with_periods_incomplete <- Time_StampeR(df_incomplete, sample_rate = 100, protocol_duration = 10, cuts = cuts, period_names = period_names)

# Warnings will indicate the discrepancy
# Period_Name for the last period may not be assigned due to insufficient data

