#####

library(tidyverse)
library(readxl)
library(plotly)
library(glue)
library(rlang)

# 
site_ts_from_xlsx_sheet <- function(compiled_path, site_id) {

  data <- read_excel(compiled_path, sheet=site_id) %>% 
    select(c('Date', 'Site', 'depth', 'Water_press', 'sensor_depth'))
  
  return(data)
  
}

fetch_water_checks <- function(meta_path, site_id){
  
  check_history <- read_excel(meta_path, sheet='Wetland_H20_level_history')
  check_history <- check_history %>% 
    filter(Site_ID == site_id)
  
  select_cols <- c(
    "Site_ID",
    "H20_date_1",
    "H20_cm_1",
    "H20_date_2",
    "H20_cm_2",
    "H20_date_3",
    "H20_cm_3",
    "H20_date_4",
    "H20_cm_4",
    "H20_date_5",
    "H20_cm_5",
    "H20_date_6",
    "H20_cm_6"
  )
  
  check_history <- check_history %>% select(all_of(select_cols))
  
  #Ensure the cm cols are numeric and date cols are datetime
  cm_cols <- grep("^H20_cm_", names(check_history), value = TRUE)
  date_cols <- grep("^H20_date_", names(check_history), value = TRUE)
  check_history <- check_history %>%
    mutate(across(all_of(cm_cols), ~ as.numeric(.))) %>% 
    mutate(across(all_of(date_cols), ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S")))
  
  # Pivot the data from wide to long format
  check_history_long <- check_history %>%
    pivot_longer(
      # Columns to pivot: all H20_date_n and H20_cm_n
      cols = starts_with("H20_date") | starts_with("H20_cm"),
      
      # Names pattern to extract the measurement number
      names_to = c(".value", "Measurement_Number"),
      
      # Regular expression to separate into .value and Measurement_Number
      names_pattern = "H20_(date|cm)_(\\d+)"
    )
  
  check_history_long <- check_history_long %>% 
    mutate('meter' = cm / 100)
  
  return(check_history_long)
}

make_site_ts <- function(site_ts, y_var, qaqc_df) {
  # Convert the y_var name to a string internally
  y_var_sym  <- rlang::ensym(y_var)
  y_var_name <- rlang::as_string(y_var_sym)
  
  site_id <- site_ts %>% 
    pull(Site) %>% 
    unique()
  
  fig <- site_ts %>%
    plot_ly(
      x = ~Date,
      # Convert the string into a plotly formula, e.g. "~WaterLevel_m"
      y = as.formula(paste0("~", y_var_name)), 
      type = "scatter",
      mode = "lines"
    ) %>%
    layout(title = glue("PT Data for Wetland {site_id}"))
  
  fig <- fig %>%
    add_trace(
      data = qaqc_df,
      x = ~date,
      y = ~meter,
      type = "scatter",
      mode = "markers",
      marker = list(color = "red", size = 10),
      name = "QA/QC"
    )
  
  fig
}
