
# Simulating Data ---------------------------------------------------------

# Packages
if (!"tidyverse" %in% .packages()) library(tidyverse)

# 1 Import Data -----------------------------------------------------------
cp_data <- read_csv("data/complete_data.csv")

# 2 Simulate triplicates -------------------------------------------------
set.seed(5) # For reproducibility

trip_data <- map2(cp_data$MEAN, cp_data$SD, rnorm, n = 3)
trip_data <- unlist(trip_data)
trip_data <- signif(trip_data, 3)

# 3 Data frame with the triplicates ---------------------------------------

main_data <- data.frame(
  MET  = rep(cp_data$MET, each = 3),
  TIME = rep(cp_data$TIME, each = 3),
  SAMPLE = rep(1:3), # Replicate number
  QT   = trip_data
)

# 3.1 Save main data
write_csv(main_data, "data/main_data.csv")
