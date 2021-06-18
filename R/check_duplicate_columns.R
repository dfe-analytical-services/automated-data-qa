source("R/0_get_file_info.R")

# Duplicated columns ---------------------------------------------------------

duplicated_cols <- data %>% 
  dplyr::select(-filters) %>% 
  t() %>% #flip the data round so cols become rows
  as.data.frame() %>% 
  janitor::get_dupes() %>% 
  dplyr::distinct() %>% 
  dplyr::summarise(dupe_count = sum(dupe_count))

duplicated_cols_line <- if(duplicated_cols %>% as.character()==0){
  "You have no duplicated columns"
} else {
  paste0("You have ", duplicated_cols %>% as.character(), " columns that match another column. Please review to see if this is correct.")
}
