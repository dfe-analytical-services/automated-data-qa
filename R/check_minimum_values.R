source("R/0_get_file_info.R")

# MINIMUM - national
#Create empty DF to bind into
minimum_results<-data.frame(filters) %>% 
  rbind("Indicator") %>% 
  rbind("Minimum") %>% 
  t() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  as.data.frame()

#Create function to loop through all indicators

for (indicator in all_of(indicators)){
  
  min_group <- data %>% 
    dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    dplyr::group_by(time_period) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    dplyr::filter(get(indicator) == min(get(indicator),na.rm=TRUE)) %>% 
    dplyr::select(all_of(filters),indicator) %>% 
    tidyr::gather(indicator, "Minimum",-all_of(filters))
  
  minimum_results <- minimum_results %>%  rbind(min_group)
}
