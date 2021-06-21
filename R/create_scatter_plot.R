source("R/0_get_file_info.R")

# Scatterplots  -----------------------------------------------------------
# Useful for YoY trend analysis/picking outliers.

# Function to create YoY scatter plot by LA, coloured by region
#NOTE you will need 
create_plot_LA <- function(data,indicator,filter_definition,time_x,time_y){
  
  data_prep_example <- data %>% 
    dplyr::filter(geographic_level == "Local authority") %>% 
    dplyr::select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>% 
    dplyr::mutate(!!indicator := as.numeric(get(indicator))) %>% 
    tidyr::spread(time_period,indicator) %>% 
    #Make sure you've defined what each filter should be
    dplyr::filter(eval(parse(text=filter_definition)))
  
  
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

}

# apply to indicator here
# 
create_plot_LA(data, 
               indicator = "average_spend", 
               filter_definition ="gender == 'Total' & school_type == 'State funded'", 
               "2021",
               "2020")