source("R/0_get_file_info.R")

#MAXIMUM - national
#Create empty DF to bind into
maximum_results<-data.frame(filters) %>% 
  rbind("Indicator") %>% 
  rbind("Maximum") %>% 
  t() %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame()

#Create function to loop through all indicators

for (indicator in all_of(indicators)){
  
  max_group <- data %>% 
    filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    group_by(time_period) %>% 
    mutate(!!indicator := as.numeric(get(indicator))) %>% 
    filter(get(indicator) == max(get(indicator),na.rm=TRUE)) %>% 
    select(all_of(filters),indicator) %>% 
    gather(indicator, "Maximum",-all_of(filters))
  
  maximum_results <- maximum_results %>%  rbind(max_group)
}
