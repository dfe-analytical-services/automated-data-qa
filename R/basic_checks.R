# RECOMMENDED BASIC AUTOMATED QA CHECKS -
library(dplyr)
library(data.table)
library(tidyr)
library(janitor)
library(plotly)
library(DT)
library(kableExtra)

# STEP 1: read in the data ------------------------------------------------
data <- data.table::fread("data/testing2.csv") #Your file path here
metadata <- data.table::fread("data/testing2.meta.csv") #Your metadata file path here


# STEP 2: Use metadata to get list of filters and indicators --------------

#Get list of indicators
indicators <-  metadata %>% 
  dplyr::filter(col_type == "Indicator") %>% 
  dplyr::pull(col_name)


#Get list of filters
filters<- data %>% 
  dplyr::select(-indicators) %>% 
  names()

#Get list of publication-specific filters
publication_filters <- metadata %>% 
  dplyr::filter(col_type == "Filter") %>% 
  dplyr::select(col_name) %>% 
  dplyr::pull(col_name)


#Get filter group combos for publication-specific filters
distinct_filter_groups <- data %>% 
  dplyr::select(dplyr::all_of(publication_filters)) %>% 
  dplyr::distinct()



# Summary stats at the highest level-------------------

#MAXIMUM - national
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


# MINIMUM - national
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


# AVERAGE - national
#Write function to pull out averages
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
average_results <- dplyr::bind_rows(average_results_list)




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
# avg_spend_outliers<- get_outliers("average_spend","Local authority",thresh = 0.1,current_time = "2021",time_compare = "2020")
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
#Add % of total cells

#anything not numeric - additional check

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
data_prep_example <- data %>%
  filter(geographic_level == "Local authority") %>%
  select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>%
  mutate(!!indicator := as.numeric(get(indicator))) %>%
  spread(time_period,indicator) %>%
  #Make sure you've defined what each filter should be
  filter(school_type == "State funded")

plot_example <- data_prep_example %>%
  plot_ly(
    type = 'scatter',
    x = ~ get(time_x),
    y = ~ get(time_y),
    color = ~region_name,
    text = ~paste("Indicator:", indicator , "<br>LA: ", la_name, '<br>', time_x, ":", get(time_x),'<br>', time_y, ":", get(time_y)),
    hoverinfo = 'text',
    mode = 'markers',
    #Need to define your filters here
    transforms = list(
      list(
        type = 'filter',
        target = ~gender,
        operation = '=',
        value = unique(data_prep_example$gender)[1])
    )) %>%
  layout(
    xaxis = list(title = time_x),
    yaxis = list(title = time_y),
    #Then define again down here
    updatemenus = list(
      list(
        y = 800,
        type = 'dropdown',
        active = 0,
        buttons = apply(as.data.frame(unique(data_prep_example$gender)), 1,
                        function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))

    )
  )

