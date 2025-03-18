
library(tidyverse)
library(readxl)
library(plotly)
library(glue)
library(rlang)

# 
site_ts_from_xlsx_sheet <- function(compiled_path, site_id) {
  
  # if site_id has a wonky slash "/" in the name, change to "." for matching sheet names
  if(grepl("/", site_id)){
    site_id <- gsub("/", ".", site_id)
  }
  
  data <- read_excel(compiled_path, sheet=site_id) %>% 
    select(c('Date', 'Site', 'depth', 'sensor_depth')) %>% 
    rename(original_depth = depth,
           Site_ID = Site) %>% 
    mutate(Site_ID = gsub("\\.", "/", Site_ID))
  
  return(data)
}

fetch_water_checks <- function(meta_path, site_id){
  
  check_history <- read_excel(meta_path, 
                              sheet='Wetland_H20_level_history',
                              na=c("", "NA", "#N/A", "N/A"))
  check_history <- check_history %>% 
    filter(Site_ID == site_id)
  
  # TODO: Update the select cols as we get more checks.
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
    mutate(across(all_of(date_cols), ~ as.POSIXct(., format = "%Y-%m-%d")))
  
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
    mutate('meter' = cm / 100) %>% 
    # !!!! NOTE: cut zeros from QAQC, many of them are not real.
    # Indicates water level below ground, but no well-sounder on date
    filter(meter != 0) 

  return(check_history_long)
}


fetch_post_process_status <- function(path, site_id){
  
  # if site_id has a wonky slash "/" in the name, change to "." for matching sheet names
  if(grepl("/", site_id)){
    site_id <- gsub("/", ".", site_id)
  }
  
  basin_id <- str_split_1(site_id, pattern="_")[1]
  
  col_spec <- c(
    "Wetland" = "text",
    "Logger WL and Measured WL Difference (m)" = "numeric",
    "WL Date" = "date",
    "Logger WL and Measured WL Difference 2 (m)" = "numeric",
    "WL Date 2" = "date",
    "Logger WL and Measured WL Difference 3 (m)" = "numeric",
    "WD Date 3" = "date", 
    "Logger or Field Measurement Logger" = 'text',
    "Bottoms out?" = "text", 
    "Bottom out depth (m)" = "numeric",
    "Notes" = "text"
  )
  
  status <- read_excel(path, 
                       sheet=paste0("Basin ", basin_id),
                       col_types = col_spec,
                       na = c("", "NA", "#N/A", "N/A")) %>% 
    # Reading all columns as "text", because of excel's wonky auto formatting. 
    # Dates were listed as integers. 
    filter(Wetland == site_id) %>% 
    rename(Site_ID = Wetland) %>% 
    select(c(Site_ID, Notes))
  
  return(status)
}

fetch_pivot_history <- function(path, site_id){
  
  select_cols <- c(
    "Site_ID",
    "P_G/L_date_1",
    "P_G_cm_1", 
    "P_L_cm_1",
    "P_G/L_date_2", 
    "P_G_cm_2",
    "P_L_cm_2", 
    "P_G/L_date_3", 
    "P_G_cm_3",
    "P_L_cm_3", 
    "P_G/L_date_4", 
    "P_G_cm_4",
    "P_L_cm_4"
  )
  
  P_G_cols <- grep("P_G_cm_", select_cols, value=TRUE)
  P_L_cols <- grep("P_L_cm_", select_cols, value=TRUE)
  date_cols <- grep("P_G/L_date_", select_cols, value=TRUE)
  
  pivot_history <- read_excel(path, sheet = "Wetland_pivot_history") %>% 
    filter(Site_ID == site_id) %>% 
    select(all_of(select_cols)) %>% 
    mutate(across(all_of(P_G_cols), ~ as.numeric(.)),
           across(all_of(P_L_cols), ~ as.numeric(.)),
           across(all_of(date_cols),  ~ as.POSIXct(., format = "%Y-%m-%d")))
  
  # For ever P_G and P_L column, calculate the corresponding "offset" column
  for(i in seq_along(P_G_cols)){
    pivot_history <- pivot_history %>% 
      mutate(!!paste0("offset_m_", i) := (
        .data[[paste0("P_L_cm_", i)]] - .data[[paste0("P_G_cm_", i)]]) / 100)
  }
  
  # Remove any columns with NA values
  pivot_history <- pivot_history %>% 
    select(where(~ !anyNA(.)))
  
  return(pivot_history)
}

