# RECOMMENDED AUTOMATED QA CHECKS -
library(dplyr)
library(data.table)
library(tidyr)
library(janitor)



# STEP 1: read in the data ------------------------------------------------
data <- fread("data/testing2.csv") #Your file path here
metadata <- fread("data/testing.meta.csv") #Your metadata file path here


# STEP 2: Use metadata to get list of filters and indicators --------------

#Get list of filters required in EES
auto_filters <- c("time_period","time_identifier","geographic_level","country_name","country_code","region_name","region_code","la_name","old_la_code","new_la_code") %>% 
  as.data.frame()

names(auto_filters)<- "col_name"

filters <- metadata %>% 
  filter(col_type == "Filter") %>% 
  select(col_name) %>% 
  rbind(auto_filters) %>% 
  pull(col_name)

distinct_filter_groups <- data %>% 
  select(all_of(filters)) %>% 
  distinct()

indicators <-  metadata %>% 
  filter(col_type == "Indicator") %>% 
  pull(col_name)


# Summary stats at the highest level-------------------

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
    filter(get(indicator) == max(get(indicator))) %>% 
    select(all_of(filters),indicator) %>% 
    gather(indicator, "Maximum",-all_of(filters))

  maximum_results <- maximum_results %>%  rbind(max_group)
}


# MINIMUM - national
#Create empty DF to bind into
minimum_results<-data.frame(filters) %>% 
  rbind("Indicator") %>% 
  rbind("Minimum") %>% 
  t() %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame()

#Create function to loop through all indicators

for (indicator in all_of(indicators)){
  
  min_group <- data %>% 
    filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    group_by(time_period) %>% 
    mutate(!!indicator := as.numeric(get(indicator))) %>% 
    filter(get(indicator) == min(get(indicator))) %>% 
    select(all_of(filters),indicator) %>% 
    gather(indicator, "Minimum",-all_of(filters))
  
  minimum_results <- minimum_results %>%  rbind(min_group)
}


# AVERAGE - national
#Create empty DF to bind into
average_results<-data.frame(
  time_period = character(),
  Indicator = character(),
  Average= double())

#Create function to loop through all indicators

for (indicator in all_of(indicators)){
  
  avg_group <- data %>% 
    filter(geographic_level == "National") %>% #Set geographic level here if you want to change to region/LA
    group_by(time_period) %>% 
    mutate(!!indicator := as.numeric(get(indicator))) %>% 
    mutate(!!indicator := mean(get(indicator))) %>% 
    select(time_period,indicator) %>% 
    distinct %>% 
    gather(indicator, "Average",-time_period)
  
  average_results <- average_results %>%  rbind(avg_group)
}



# Extreme values ----------------------------------------------------------




# - Sums (YoY, Geographies, Filters)

# - Duplicated cols (mainly for indicators for users)

# Missing data checks, counting suppressed cells --------------------------
# How many cells are suppressed?
suppressed_cell_count <- data %>%  
  select(-filters) %>%
  unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  filter(. %in% c("z","c",":","~")) #also include non-standard ones here? Could add warning to tell analysts to stick to GSS guidance


# Duplicated rows ---------------------------------------------------------
# IGNORING filter labels, are there any rows that are identical?
# Might be fine but worth double checking
duplicated_rows <- data %>% 
  select(-filters) %>% 
  get_dupes()


# - Scatterplot stuff