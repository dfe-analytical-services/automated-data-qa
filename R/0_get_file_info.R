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
