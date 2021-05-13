# RECOMMENDED BASIC AUTOMATED QA CHECKS -
library(dplyr)
library(data.table)
library(tidyr)
library(janitor)
library(plotly)
library(DT)
library(kableExtra)

# STEP 1: read in the data ------------------------------------------------
data <- fread("data/testing2.csv") #Your file path here
metadata <- fread("data/testing2.meta.csv") #Your metadata file path here


# STEP 2: Use metadata to get list of filters and indicators --------------

#Get list of indicators
indicators <-  metadata %>% 
  filter(col_type == "Indicator") %>% 
  pull(col_name)


#Get list of filters
filters<- data %>% 
  select(-indicators) %>% 
  names()

#Get list of publication-specific filters
publication_filters <- metadata %>% 
  filter(col_type == "Filter") %>% 
  select(col_name) %>% 
  pull(col_name)


#Get filter group combos for publication-specific filters
distinct_filter_groups <- data %>% 
  select(all_of(publication_filters)) %>% 
  distinct()



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
    filter(get(indicator) == max(get(indicator),na.rm=TRUE)) %>% 
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
    filter(get(indicator) == min(get(indicator),na.rm=TRUE)) %>% 
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
    mutate(!!indicator := mean(get(indicator),na.rm=TRUE)) %>% 
    select(time_period,indicator) %>% 
    distinct %>% 
    gather(indicator, "Average",-time_period)
  
  average_results <- average_results %>%  rbind(avg_group)
}



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
# avg_spend_outliers<- get_outliers("average_spend","Local authority",thresh = 0.2,current_time = "2021",time_compare = "2020")
# 
# above_threshold <- avg_spend_outliers %>%  filter(flag_big == "TRUE")
# 
# below_threshold<- avg_spend_outliers %>%  filter(flag_small == "TRUE")


# Missing data checks, counting suppressed cells --------------------------
# How many cells are suppressed?
suppressed_cell_count <- data %>%  
  select(-all_of(filters)) %>%
  unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  filter(. %in% c("z","c",":","~")) #also include non-standard ones here? Could add warning to tell analysts to stick to GSS guidance


# Duplicated rows ---------------------------------------------------------
# IGNORING filter labels, are there any rows that are identical?
# Might be fine but worth double checking
duplicated_rows <- data %>% 
  select(-filters) %>% 
  get_dupes() %>% 
  distinct()


# Duplicated columns ------------------------------------------------------
# IGNORING column headings, are there any columns that are identical?
duplicated_cols <- data %>% 
  select(-filters) %>% 
  t() %>% #flip the data round so cols become rows
  as.data.frame() %>% 
  get_dupes() %>% 
  distinct() %>% 
  summarise(dupe_count = sum(dupe_count))

duplicated_cols_line <- if(duplicated_cols %>% as.character()==0){
  "You have no duplicated columns"
} else {
  paste0("You have ", duplicated_cols %>% as.character(), " columns that match another column. Please review to see if this is correct.")
}



# BELOW CHECKS NOT GENERALISED BUT CAN BE TEMPLATES -----------------------

# Sum checks --------------------------------------------------------------

# Function to compare LA subtotals match region totals.
# Outputs a table of mismatches
check_LA_region_totals <- function(indicator){
  
data_region <- data %>% 
  #filter for regional level data
  filter(geographic_level == "Regional") %>% 
  #select all descriptive variables and indicator of choice
  select(time_period,region_name,region_code,all_of(publication_filters),indicator) %>% 
  #make sure indicator is numeric
  mutate(!!indicator := as.numeric(get(indicator))) %>% 
  #order by year, region code then custom filter groups
  arrange(time_period,region_code,get(publication_filters))

data_la_aggregate <- data %>% 
  #filter for LA level data
  filter(geographic_level == "Local authority") %>% 
  #select descriptive varaibles and indicator of choice
  select(time_period,region_name,region_code,all_of(publication_filters),indicator) %>% 
  #group by all descriptive variables
  group_by(across(-c(indicator))) %>% 
  #summarise number of pupils for above groupings
  summarise(!!indicator := sum(as.numeric(get(indicator),na.rm = TRUE))) %>% 
  #order by year, region code then custom filter groups
  arrange(time_period,region_code,get(publication_filters))

#check and see if the tables differ - any rows will show which region does not add up
compare <- setdiff(data_region,data_la_aggregate)



if (nrow(compare) == 0) {
  paste0("* The LA subtotals for ", indicator, " match to the region totals")
} else {
  DT::datatable(compare)
}
}


#E.g. of how you'd apply it here
# avg_spend_LA_region_check <- check_LA_region_totals("average_spend")
# avg_grade_LA_region_check <- check_LA_region_totals("average_grade")

# Scatterplots  -----------------------------------------------------------
# Useful for YoY trend analysis/picking outliers:

# Function to create YoY scatter plot by LA, coloured by region

create_plot_LA <- function(data,indicator,time_x,time_y){
  
data_prep_example <- data %>% 
  filter(geographic_level == "Local authority") %>% 
  select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>% 
  mutate(!!indicator := as.numeric(get(indicator))) %>% 
  spread(time_period,indicator) %>% 
  #Make sure you've defined what each filter should be
  filter(gender == "Total",
         school_type == "State funded")


plot_example <- data_prep_example %>%
  plot_ly(
    type = 'scatter', 
    x = ~ get(time_x), 
    y = ~ get(time_y),
    color = ~region_name,
    text = ~paste("Indicator:", indicator , "<br>LA: ", la_name, '<br>', time_x, ":", get(time_x),'<br>', time_y, ":", get(time_y)),
    hoverinfo = 'text',
    mode = 'markers') %>% 
  layout(
    xaxis = list(title = time_x),
    yaxis = list(title = time_y))

plot_example
}

# apply to indicator here
# 
# create_plot_LA(data, "average_spend", "2021","2020")



# Scatter plot with dropdowns for a filter ------------------------------------
# 
# data_prep_example <- data %>%
#   filter(geographic_level == "Local authority") %>%
#   select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>%
#   mutate(!!indicator := as.numeric(get(indicator))) %>%
#   spread(time_period,indicator) %>%
#   #Make sure you've defined what each filter should be
#   filter(school_type == "State funded")
# 
# plot_example <- data_prep_example %>%
#   plot_ly(
#     type = 'scatter',
#     x = ~ get(time_x),
#     y = ~ get(time_y),
#     color = ~region_name,
#     text = ~paste("Indicator:", indicator , "<br>LA: ", la_name, '<br>', time_x, ":", get(time_x),'<br>', time_y, ":", get(time_y)),
#     hoverinfo = 'text',
#     mode = 'markers',
#     #Need to define your filters here
#     transforms = list(
#       list(
#         type = 'filter',
#         target = ~gender,
#         operation = '=',
#         value = unique(data_prep_example$gender)[1])
#     )) %>%
#   layout(
#     xaxis = list(title = time_x),
#     yaxis = list(title = time_y),
#     #Then define again down here
#     updatemenus = list(
#       list(
#         y = 800,
#         type = 'dropdown',
#         active = 0,
#         buttons = apply(as.data.frame(unique(data_prep_example$gender)), 1,
#                         function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
# 
#     )
#   )
# 
