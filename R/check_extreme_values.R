source("R/0_get_file_info.R")

# Extreme values ----------------------------------------------------------

# Set threshold based on last time period's values

get_outliers <- function(indicator, geog_level, thresh, current_time,time_compare){
  data %>% 
    #Set geographic level here if you want to change to region/LA
    dplyr::filter(geographic_level == geog_level) %>% 
    dplyr::group_by(time_period) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    dplyr::select(dplyr::all_of(filters),indicator) %>% 
    tidyr::spread(time_period,indicator) %>% 
    dplyr::mutate(thresh_indicator_big = get(time_compare) * (1+thresh),
           thresh_indicator_small = get(time_compare) * (1-thresh),
           flag_big = get(current_time) >= thresh_indicator_big,
           flag_small = get(current_time) <= thresh_indicator_small)}


#Demo how you'd use the function here:         
avg_spend_outliers<- get_outliers("average_spend","Local authority",thresh = 0.1,current_time = "2021",time_compare = "2020")

above_threshold <- avg_spend_outliers %>%  dplyr::filter(flag_big == "TRUE")

below_threshold<- avg_spend_outliers %>%  dplyr::filter(flag_small == "TRUE")