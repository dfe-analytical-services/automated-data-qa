source("R/0_get_file_info.R")

# Duplicated rows ---------------------------------------------------------
# IGNORING filter labels, are there any rows that are identical?
# Might be fine but worth double checking
duplicated_rows <- data %>% 
  select(-filters) %>% 
  get_dupes() %>% 
  distinct()
