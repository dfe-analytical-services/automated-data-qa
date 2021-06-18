source("R/0_get_file_info.R")

# AVERAGE - national
#Create empty DF to bind into
average_results<-data.frame(
  time_period = character(),
  Indicator = character(),
  Average= double())

#Create function to loop through all indicators

for (indicator in all_of(indicators)){
  
  avg_group <- data %>% 
    dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    dplyr::group_by(time_period) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    dplyr::mutate(!!indicator := mean(get(indicator),na.rm=TRUE)) %>% 
    dplyr::select(time_period,indicator) %>% 
    dplyr::distinct %>% 
    tidyr::gather(indicator, "Average",-time_period)
  
  average_results <- average_results %>%  rbind(avg_group)
}
