source("R/0_get_file_info.R")

#Write function to pull out minimum
get_min <- function(indicator) {
  
  data %>% 
    dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    dplyr::group_by(time_period) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    dplyr::filter(get(indicator) == min(get(indicator),na.rm=TRUE)) %>% 
    dplyr::select(all_of(filters),indicator) %>% 
    tidyr::gather(indicator, "Minimum",-all_of(filters))
}

#Get list of outputs
min_results_list <- lapply(indicators,get_min)

#Create dataframe from outputs
min_results <- dplyr::bind_rows(min_results_list)
