source("R/0_get_file_info.R")


# Scatter plot with dropdowns for a filter ------------------------------------
#

create_dropdown_plot_LA <- function(data,indicator,filter_definition,filter_dropdown_choice,time_x,time_y){

data_prep_example <- data %>%
  filter(geographic_level == "Local authority") %>%
  select(time_period,region_name,la_name,all_of(publication_filters),indicator) %>%
  mutate(!!indicator := as.numeric(get(indicator))) %>%
  spread(time_period,indicator) %>%
  #Make sure you've defined what each filter should be
  filter(eval(parse(text=filter_definition)))

data_prep_example %>%
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
        target = ~get(filter_dropdown_choice),
        operation = '=',
        value = eval(parse(text = paste0("unique(data_prep_example$", filter_dropdown_choice, ")[1]")))
       ))) %>%
  layout(
    xaxis = list(title = time_x),
    yaxis = list(title = time_y),
    #Then define again down here
    updatemenus = list(
      list(
        y = 800,
        type = 'dropdown',
        active = 0,
        buttons = apply(as.data.frame(eval(parse(text = paste0("unique(data_prep_example$", filter_dropdown_choice,")")))), 1,
                        function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
      
    )
  )
}

#How to apply
create_dropdown_plot_LA(data,
                        indicator = "average_spend",
                        filter_definition = "school_type == 'State funded'",
                        filter_dropdown_choice = "gender",
                        time_x = "2021",
                        time_y = "2020")
