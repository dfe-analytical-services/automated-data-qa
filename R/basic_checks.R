# RECOMMENDED AUTOMATED QA CHECKS -
library(dplyr)
library(data.table)
library(tidyr)
library(janitor)
library(plotly)



# STEP 1: read in the data ------------------------------------------------
data <- fread("data/testing2.csv") #Your file path here
metadata <- fread("data/testing.meta.csv") #Your metadata file path here


# STEP 2: Use metadata to get list of filters and indicators --------------

#Get list of filters required in EES
auto_filters <- c("time_period","time_identifier","geographic_level","country_name","country_code","region_name","region_code","la_name","old_la_code","new_la_code") 

#Get list of publication-specific filters
publication_filters <- metadata %>% 
  filter(col_type == "Filter") %>% 
  select(col_name) %>% 
  pull(col_name)

#Join together
filters <- c(publication_filters, auto_filters)

#Get filter group combos for publication-specific filters
distinct_filter_groups <- data %>% 
  select(all_of(publication_filters)) %>% 
  distinct()

#Get list of indicators
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


# Duplicated columns ------------------------------------------------------
# IGNORING column headings, are there any columns that are identical?
duplicated_cols <- data %>% 
  select(-filters) %>% 
  t() %>% #flip the data round so cols become rows
  as.data.frame() %>% 
  get_dupes()


# Scatterplots  -----------------------------------------------------------
# Useful for YoY trend analysis/picking outliers:

# Function to create YoY scatter plot by LA, coloured by region

create_plot_LA <- function(data,indicator,time_x,time_y){
  
data_prep_example <- data %>% 
  filter(geographic_level == "Local authority") %>% 
  select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>% 
  mutate(!!indicator := as.numeric(get(indicator))) %>% 
  spread(time_period,indicator) %>% 
  #need to generalise this??
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
create_plot_LA(data,"number_on_roll", "2021", "2020")

create_plot_LA(data, "average_spend", "2021","2020")



# TESTING - add dropdowns to save that fitering step? ------------------------------------

data_prep_example <- data %>% 
  filter(geographic_level == "Local authority") %>% 
  select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>% 
  mutate(!!indicator := as.numeric(get(indicator))) %>% 
  spread(time_period,indicator)


plot_example <- data_prep_example %>%
  plot_ly(
    type = 'scatter', 
    x = ~ get(time_x), 
    y = ~ get(time_y),
    color = ~region_name,
    text = ~paste("Indicator:", indicator , "<br>LA: ", la_name, '<br>', time_x, ":", get(time_x),'<br>', time_y, ":", get(time_y)),
    hoverinfo = 'text',
    mode = 'markers',
    transforms = list(
      list(
        type = 'filter',
        target = ~gender,
        operation = '=',
        value = unique(data_prep_example$gender)[1]),
      list(
        type = 'filter',
        target = ~school_type,
        operation = '=',
        value = unique(data_prep_example$school_type)[1]
      )
    )) %>% 
  layout(
    xaxis = list(title = time_x),
    yaxis = list(title = time_y),
    updatemenus = list(
      list(
        y = 1000, #trying to fix the y location for this! Not working atm...
        type = 'dropdown',
        active = 0,
        buttons = apply(as.data.frame(unique(data_prep_example$school_type)), 1,
                        function(x) list(method = 'restyle',args = list('transforms[1].value',x),label = x))),
      list(
        y = 800,
        type = 'dropdown',
        active = 0,
        buttons = apply(as.data.frame(unique(data_prep_example$gender)), 1, 
                        function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
      
    )
  )

