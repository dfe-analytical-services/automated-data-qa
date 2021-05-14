source("R/0_get_file_info.R")

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
avg_spend_LA_region_check <- check_LA_region_totals("average_spend")
avg_grade_LA_region_check <- check_LA_region_totals("average_grade")