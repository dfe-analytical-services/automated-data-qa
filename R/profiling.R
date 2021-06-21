source("R/0_get_file_info.R")

library(profvis)

#Profiling R code - what's quicker, lapply or for loop?

#Lapply
profvis({
  get_averages <- function(indicator) {
    
    data %>% 
      dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
      dplyr::group_by(time_period) %>% 
      dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
      dplyr::mutate(!!indicator := mean(get(indicator),na.rm=TRUE)) %>% 
      dplyr::select(time_period,indicator) %>% 
      dplyr::distinct() %>% 
      tidyr::gather(indicator, "Average",-time_period)
  }
  
  #Get list of outputs
  average_results_list <- lapply(indicators,get_averages)
  
  #Create dataframe from outputs
  average_results<- dplyr::bind_rows(average_results_list)
})



#For loop
profvis({
  # AVERAGE - national
  #Create empty DF to bind into
  average_results<-data.frame(
    time_period = character(),
    Indicator = character(),
    Average= double())
  
  for (indicator in all_of(indicators)){
    
    avg_group <- data %>% 
      dplyr::filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
      dplyr::group_by(time_period) %>% 
      dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
      dplyr::mutate(!!indicator := mean(get(indicator),na.rm=TRUE)) %>% 
      dplyr::select(time_period,indicator) %>% 
      dplyr::distinct() %>% 
      tidyr::gather(indicator, "Average",-time_period)
    
    average_results <- average_results %>%  rbind(avg_group)
  }
})

