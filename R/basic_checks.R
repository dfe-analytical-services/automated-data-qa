# RECOMMENDED AUTOMATED QA CHECKS -


library(dplyr)
library(readr)


# STEP 1: read in the data ------------------------------------------------
data <- fread("data/testing.R")




# - Summary stats at the highest level
# For filter/indicator groups - min/max/averages?


# - Extreme values
# - Sums (YoY, Geographies, Filters)
# - Missing data checks, count the suppressed cells
# - Duplicated cols (mainly for indicators for users)
# - Duplicated rows
# - Scatterplot stuff