source("R/0_get_file_info.R")

# Extreme values ----------------------------------------------------------

# Set threshold based on last time period's values

get_outliers <- function(indicator, geog_level, thresh, current_time,time_compare){
  data %>% 
    #Set geographic level here if you want to change to region/LA
    filter(geographic_level == geog_level) %>% 
    group_by(time_period) %>% 
    mutate(!!indicator := as.numeric(get(indicator))) %>% 
    select(all_of(filters),indicator) %>% 
    spread(time_period,indicator) %>% 
    mutate(thresh_indicator_big = get(time_compare) * (1+thresh),
           thresh_indicator_small = get(time_compare) * (1-thresh),
           flag_big = get(current_time) >= thresh_indicator_big,
           flag_small = get(current_time) <= thresh_indicator_small)}


#Demo how you'd use the function here:         
avg_spend_outliers<- get_outliers("average_spend","Local authority",thresh = 0.1,current_time = "2021",time_compare = "2020")

above_threshold <- avg_spend_outliers %>%  filter(flag_big == "TRUE")

below_threshold<- avg_spend_outliers %>%  filter(flag_small == "TRUE")