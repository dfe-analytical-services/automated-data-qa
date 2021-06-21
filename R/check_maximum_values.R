source("R/0_get_file_info.R")

#Write function to pull out maximum
get_max <- function(indicator) {
  
  data %>% 
    dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    dplyr::group_by(time_period) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    dplyr::filter(get(indicator) == max(get(indicator),na.rm=TRUE)) %>% 
    dplyr::select(all_of(filters),indicator) %>% 
    tidyr::gather(indicator, "Maximum",-all_of(filters))
}

#Get list of outputs
max_results_list <- lapply(indicators,get_max)

#Create dataframe from outputs
max_results <- dplyr::bind_rows(max_results_list)
