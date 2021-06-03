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