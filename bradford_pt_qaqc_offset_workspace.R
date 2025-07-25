#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library('tidyverse')
library('plotly')
library('glue')
library('readxl')

# Get functions
source("./scripts/qaqc_functions.R")
source("./scripts/data_read_functions.R")

# Path to original data containing depth and water depth
compiled_path <-'./data/compiled_stage_JM_2.xlsx'
# Path to wetland well metadata.
# Contains field measurements for water level and well dimensions 
meta_data_path <- './data/Wetland_well_metadata_JM.xlsx'
status_path <- './data/Post_Processing_Well_Status.xlsx'


# unique_wetland_wells <- read_excel(meta_data_path, sheet="Wetland_and_well_info") %>% 
#   pull('Site_ID') %>% 
#   unique()
# 
# print(unique_wetland_wells)

# Make output dataframe and define columns
output_data <- tibble(
  Date = as.POSIXct(character()), 
  Site_ID = character(),
  sensor_depth = numeric(),
  original_depth = numeric(),
  revised_depth = numeric(), 
  offset_value = numeric(),
  offset_version = character(),
  flag = int(),
  notes = character()
)

output_checks <- tibble(
  version = character(),
  date = as.POSIXct(character()),
  diff = numeric(),
  field_check_m = numeric(),
  logger_val_m = numeric(),
  Site_ID = character()
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Priority Wetlands -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sunita: !!!6_93, 14_612, 5a_582, !!!14_500, !!!15_409 and 13_267

# AJ:6_300; !!!6_20; !!!15_409; !!!15_4; 15_268; 14/9_601; 
# 14/9_527; 14/9_168; !!!14_500; !!!14_115; !!!13_410; 13_274; 
# 3_34; 3_311; !!!9_332; !!!5_510

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I) Site: 13_263 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes form the field sheet
#4/14/22: While attempting to move well further into wetland 
# (because location was clearcut- see note from last visit) on 4/14/22, 
#the well snapped. Came back on 4/15/22 and moved well to a new location deeper 
#in the wetland (29.8678961, -82.1999974, L to P 214 cm, P to G 168 cm, H20 23 cm).

## -------- A Read the site data/metadata -----------------
site <- "13_263"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!!
offset_names_to_use1 <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use1 <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use1 <- c(offset_names_to_use1, offset_dates_to_use1)
offsets_to_use1 <- pivot_history %>% 
  select(all_of(offset_cols_to_use1))
offset_vals_use1 <- offsets_to_use1 %>% select(all_of(offset_names_to_use1))

### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use2 <- "Custom1"
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use3 <- "Custom2"

new_offset1 <- offset_vals_use1 %>%  unlist() %>% mean(na.rm = TRUE)
new_offset2 <- 0.53
new_offset3 <- 0.46

data_full <- data_full %>% 
  # Apply offset #1 prior to January 15th 2022
  # Apply offset #2 between January 15th 2022 and April 15th 2022
  # Apply offset #3 after April 16th 2022
  # Delete data in these ranges [January 15th 2022-January 16th 2022] AND [April 14th 2022 - April 16th 2022]
  filter(
    !(Date >= as.Date("2022-01-15") & Date <= as.Date("2022-01-16")),
    !(Date >= as.Date("2022-04-14") & Date <= as.Date("2022-04-16"))
  ) %>%
  mutate(
    offset_version = case_when(
      Date < as.Date("2022-01-15") ~ offset_names_to_use1,  # offset #1
      Date >= as.Date("2022-01-17") & Date <= as.Date("2022-04-13") ~ offset_names_to_use2,  # offset #2
      Date >= as.Date("2022-04-17") ~ offset_names_to_use3,  # offset #3
      TRUE ~ NA_character_
    ),
    offset_value = case_when(
      Date < as.Date("2022-01-15") ~ new_offset1,
      Date >= as.Date("2022-01-17") & Date <= as.Date("2022-04-13") ~ new_offset2,
      Date >= as.Date("2022-04-17") ~ new_offset3,
      TRUE ~ NA_real_
    ),
    flag = case_when(
      Date < as.Date("2022-01-15") ~ 0,
      Date >= as.Date("2022-01-17") & Date <= as.Date("2022-04-13") ~ 1,
      Date >= as.Date("2022-04-17") ~ 0,
    ),  # or vary by range if desired
    notes = case_when(
      Date < as.Date("2022-01-15") ~ "Original Well",
      Date >= as.Date("2022-01-17") & Date <= as.Date("2022-04-13") ~ "PT Knocked over in wetland",
      Date >= as.Date("2022-04-17") ~ "New Well",
      TRUE ~ NA_character_
    ),
    revised_depth = sensor_depth - offset_value
  )

## ------- D Plot the data with a revised offset -----------------------

# Remove anomalous values
data_full <- anomaly_remover(data_full, revised_depth_col='revised_depth')
make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.60,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
      )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use1, offsets_to_use2, new_offset1, new_offset2,
   offset_cols_to_use1, offset_cols_to_use2, offset_dates_to_use1,
   offset_dates_to_use2, offset_names_to_use1, offset_names_to_use2,
   offset_vals_use1, offset_vals_use2,
   new_offset3, offset_names_to_use3)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II) Site: 13_267 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "13_267"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

# NOTE: Offset version #1 is obviously wrong for this well
# - Just use offset version #2 for the entire timeseries
print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
# Choose offset "offset_m_2"
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
# offset_names_to_use <- all_offset_names[all_offset_names %in% c("offset_m_2", 'offset_m_3')]
# offset_dates_to_use <- all_offset_dates[all_offset_dates %in% c("P_G/L_date_2", "P_G/L_date_3")]

offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

if (length(offset_names_to_use) > 1) {
  offset_string <- paste0(offset_names_to_use, collapse = " AND ")
} else {
  offset_string <- offset_names_to_use
}

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_string,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_           
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note bottoming out-depth flag=2
data_full <- data_full %>% 
  mutate(
    flag = if_else(
      revised_depth <= -0.95,
      2,
      flag
    )
)
  

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, status, 
   checks_final, data_full, data_out) 
rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III) Site: 13_271 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "13_271"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

# - Just use offset version #2 for the entire timeseries
#   offset V2 agrees better with field checks and other offsets compared to V1
print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
# Choose offset "offset_m_2"
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_          
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.7,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )


checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 
rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV) Site: 13_274 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "13_274"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)

not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

# - Just use offset version #2 for the entire timeseries
#   offset V2 agrees with field checks offset V1 is clearly wrong
print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
# Choose offset "offset_m_2"
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_          
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')
make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.86,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )


checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 
rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V) Site: 13_410 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "13_410"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

# !!!!! Offset Verion #1 is seriously wrong. Use Version #2 instead
print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
# Choose offset "offset_m_2"
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_           
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth flag=2
data_full <- data_full %>% 
  mutate(
    flag = if_else(
      revised_depth <= -0.66,
      2,
      flag
    )
  )


data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VI) Site: 14/9_168-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14/9_168"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
# Choose offset "offset_m_1"
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_           
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover function
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.65,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VII) Site: 14/9_527-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14/9_527"

data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_           
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed out data with flag =2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.74,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VIII) Site: 14/9_601-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14/9_601"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_           
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: Well does not bottom out, bottoming out depth TBD

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IX) Site: 14_115-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_115"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_        
  )

## ------- D Plot the data with a revised offset -----------------------

# Anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.40,
    2, 
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# X) Site: 14_15-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_15"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_         
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.355,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XI) Site: 14_418-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_418"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
# Chose offset V2, becuase it matched checks better than V1!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_       
  )

## ------- D Plot the data with a revised offset -----------------------

# anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.755,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XII) Site: 14_500-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_500"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_   
  )

## ------- D Plot the data with a revised offset -----------------------

# Anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')
make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.525,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XIII) Site: 14_538-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_538"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth", "original_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_    
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: No apparent bottomed out data for this well bottom-out depth TBD

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XIV) Site: 14_610-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_610"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!!
offset_names_to_use1 <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use1 <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use1 <- c(offset_names_to_use1, offset_dates_to_use1)
offsets_to_use1 <- pivot_history %>% 
  select(all_of(offset_cols_to_use1))
offset_vals_use1 <- offsets_to_use1 %>% select(all_of(offset_names_to_use1))

### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
new_offset1 <- offset_vals_use1 %>%  unlist() %>% mean(na.rm = TRUE)


data_full <- data_full %>% 
  # Apply offset #1 prior to October 18th 2022 and after June 5th 2023
  # Delete data between October 18th 2022 and after June 5th 2023
  mutate(
    offset_version = case_when(
      Date <  as.Date("2022-10-18") ~ offset_names_to_use1,
      Date >  as.Date("2023-06-06") ~ offset_names_to_use1,
      TRUE                          ~ NA_character_
    ),
    offset_value = case_when(
      Date <  as.Date("2022-10-18") ~ new_offset1,
      Date >  as.Date("2023-06-06") ~ new_offset1,
      TRUE                          ~ NA_real_
    ),
    flag = case_when(
      Date <  as.Date("2022-10-18") ~ 0,
      Date >  as.Date("2023-06-06") ~ 0,
      TRUE                          ~ 5
    ),
    notes = case_when(
      Date <  as.Date("2022-10-18") ~ NA_character_,
      Date >  as.Date("2023-06-06") ~ NA_character_,
      TRUE                          ~ "Equipment malfunction! Data deleted"
    ),
    revised_depth = sensor_depth - offset_value
  ) 

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth with flag =2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.575,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use1, new_offset1, offset_cols_to_use1, 
   offset_dates_to_use1, offset_names_to_use1, offset_vals_use1)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XV) Site: 14_612-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_612"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_    
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# flag = 2 for bottoming out depth
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.71,
    2,
    flag
  )
)
# Note the bottoming out depth

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XVI) Site: 14_616-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "14_616"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_    
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.80,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XVIII) Site: 15_268-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "15_268"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_  
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.59, 
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XIX) Site: 15_4-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "15_4"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_  
)
    

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Mark bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.68,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XX) Site: 15_409-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "15_409"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag the bottomed-out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.525,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXI) Site: 15_516-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "15_516"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.55,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXII) Site: 3_173-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_173"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)


## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag the bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.47,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXIII) Site: 3_21-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_21"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
# Offset v3 is clearly wrong
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag bottomed out data with flag =2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.765,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXIV) Site: 3_23-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_23"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = if_else(Date >= '2024-05-03', 1, 0),                      
    notes          = if_else(Date >= '2024-05-03', 'Post May 3rd 2024, bad field check agreement.', NA_character_)
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: well does not bottom out, TBD for depth limit flag. 

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXV) Site: 3_244-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_244"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = if_else(Date >= '2023-06-23', 1, 0),                      
    notes          = if_else(Date >= '2023-06-23', 'Limited field checks. Bottom-out depth change? Need more QAQC', NA_character_)    
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag the bottomed out data = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.72,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXVI) Site: 3_311-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_311"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.68,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXVII) Site: 3_34-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_34"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
# V3 offset is clearly wrong!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))

offset_names_to_use1 <- offset_names_to_use
new_offset1 <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

offset_names_to_use2 <- 'Estimated offset to fix download jump'
new_offset2 <- 0.50

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>% 
  mutate(
    offset_version = case_when(
      Date <=  as.Date("2024-05-02") ~ offset_names_to_use1,
      Date >=  as.Date("2024-05-04") ~ offset_names_to_use2,
      TRUE                          ~ NA_character_  # "Between" dates get NA 
    ),
    offset_value = case_when(
      Date <=  as.Date("2024-05-02") ~ new_offset1,
      Date >=  as.Date("2024-05-04") ~ new_offset2,
      TRUE                          ~ NA_real_
    ),
    flag = case_when(
      Date <=  as.Date("2024-05-02") ~ 0,
      Date >=  as.Date("2024-05-04") ~ 1,
      TRUE                          ~ NA_real_
    ),
    notes = case_when(
      Date <=  as.Date("2024-05-02") ~ NA_character_,
      Date >=  as.Date("2024-05-04") ~ 'Data shifted on download date. Estimated new offset, but bottom out depth changes.',
      TRUE                          ~ NA_character_
    ),
    revised_depth = case_when(
      Date <  as.Date("2024-05-02") ~ sensor_depth - offset_value,
      Date >  as.Date("2024-05-04") ~ sensor_depth - offset_value,
      TRUE ~ NA_real_
    ),
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.44, 
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset1, new_offset2, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use,
   offset_names_to_use1, offset_names_to_use2)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXVIII) Site: 3_638-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "3_638"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))

offset_names_to_use1 <- offset_names_to_use
new_offset1 <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

offset_names_to_use2 <- 'Estimated offset to fix download jump'
new_offset2 <- 0.52

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>% 
  mutate(
    offset_version = case_when(
      Date <=  as.Date("2024-05-02") ~ offset_names_to_use1,
      Date >=  as.Date("2024-05-04") ~ offset_names_to_use2,
      TRUE                          ~ NA_character_  # "Between" dates get NA 
    ),
    offset_value = case_when(
      Date <=  as.Date("2024-05-02") ~ new_offset1,
      Date >=  as.Date("2024-05-04") ~ new_offset2,
      TRUE                          ~ NA_real_
    ),
    flag = case_when(
      Date <=  as.Date("2024-05-02") ~ 0,
      Date >=  as.Date("2024-05-04") ~ 1,
      TRUE                          ~ NA_real_
    ),
    notes = case_when(
      Date <=  as.Date("2024-05-02") ~ NA_character_,
      Date >=  as.Date("2024-05-04") ~ 'Data shifted on download date. Estimated new offset, but bottom out depth changes.',
      TRUE                          ~ NA_character_
    ),
    revised_depth = case_when(
      Date <  as.Date("2024-05-02") ~ sensor_depth - offset_value,
      Date >  as.Date("2024-05-04") ~ sensor_depth - offset_value,
      TRUE ~ NA_real_
    ),
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: timeseries does not bottom out, so bottom out depth is TBD.

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 
rm(offsets_to_use, new_offset1, new_offset2, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use,
   offset_names_to_use1, offset_names_to_use2)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXIX) Site: 5_161-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_161"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottom-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.715,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXX) Site: 5_321-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_321"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: the well soes not appear to bottom out. Bottoming out depth is TBD. 

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXI) Site: 5_510-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_510"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag the bottomed-out data = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.615,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXII) Site: 5_546-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_546"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottoming out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -1.055,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXIII) Site: 5_560-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_560"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: there is not a clear bottom-out depth for this site. TBD. 

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXIV) Site: 5_573-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_573"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottom-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.69,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXV) Site: 5_597 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5_597"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.63,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXVI) Site: 5a_550 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5a_550"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.26,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXVII) Site: 5a_582 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5a_582"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Remove bottomed-out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.34,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXVIII) Site: 5a_598 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "5a_598"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.475,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXXIX Site: 6_20-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6_20"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: there's no apparent bottom-out for this well flag=2 depth TBD.

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XL) Site: 6_300-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6_300"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottom-out depth with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.825,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLI Site: 6_629-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6_629"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Mark bottomed out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -1.185,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLII) Site: 6_93-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6_93"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: bottomed-out data is not clear, flag=2 depth is TBD.

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLIII) Site: 6a_17-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6a_17"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE: bottom out (flag=2) depth is still TBD.

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLIV) Site: 6a_530-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "6a_530"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed-out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.68,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLV) Site: 7_243-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "7_243"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Mark the bottom-out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.74,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLVI) Site: 7_341-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "7_341"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# NOTE there is not a clear bottom-out depth for flag = 2, TBD

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLVII) Site: 7_622-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "7_622"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag =2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.31,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLVIII) Site: 7_626-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "7_626"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)


## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed-out data with flag = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.52,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLIX) Site: 9_332-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "9_332"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)
# PICK OFFSET HERE
### !!!!!!! offset V2 !!!!!!!!!!!!!!!!!!
offset_names_to_use1 <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use1 <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use1 <- c(offset_names_to_use1, offset_dates_to_use1)
offsets_to_use1 <- pivot_history %>% 
  select(all_of(offset_cols_to_use1))
offset_vals_use1 <- offsets_to_use1 %>% select(all_of(offset_names_to_use1))

new_offset1 <- offset_vals_use1 %>%  unlist() %>% mean(na.rm = TRUE)

### !!!!!!! Needed to find new offset !!!!!!!!!!!!!!!!!
offset_names_to_use2 <- "Estimated offset A"
offset_names_to_use3 <- "Estimated offset B"
offset_names_to_use4 <- "Estimated offset C"
new_offset2 <- -0.13
new_offset3 <- -0.18 
new_offset4 <- 0.80

# Apply the chosen offset to the entire timeseries
# Apply offset #1 prior to August 7th 2023
# Apply offset #2 between August 7th 2023 and November 10th 2023
# Apply offset #3 between November 10th 2023 and April 24th 2024
# Apply offset #4 After April 24th 2024
# Delete data in between
data_full <- data_full %>%
  mutate(
    # 1) Set which offset version applies to each date
    offset_version = case_when(
      Date < as.Date("2023-08-06") ~ offset_names_to_use1,
      Date > as.Date("2023-08-10") & Date < as.Date("2023-11-10") ~ offset_names_to_use2,
      Date > as.Date("2023-11-10") & Date < as.Date("2024-04-24") ~ offset_names_to_use3,
      Date > as.Date("2024-04-24") ~ offset_names_to_use4,
      TRUE ~ NA_character_
    ),
    
    # 2) The numerical offset value for each date
    offset_value = case_when(
      Date < as.Date("2023-08-06") ~ new_offset1,
      Date > as.Date("2023-08-10") & Date < as.Date("2023-11-10") ~ new_offset2,
      Date > as.Date("2023-11-10") & Date < as.Date("2024-04-24") ~ new_offset3,
      Date > as.Date("2024-04-24") ~ new_offset4,
      TRUE ~ NA_real_
    ),
    
    # 3) (Optional) Define flags or notes similarly
    flag = case_when(
      Date < as.Date("2023-08-06") ~ 0,
      Date >= as.Date("2023-08-10") & Date < as.Date("2023-11-10") ~ 1,
      Date >= as.Date("2023-11-10") & Date < as.Date("2024-04-24") ~ 1,
      Date >= as.Date("2024-04-24") ~ 0,
      TRUE ~ NA_real_
    ),
    notes = case_when(
      offset_version == offset_names_to_use1 ~ NA_character_,
      offset_version == offset_names_to_use2 ~ "Well PVC broken PT laying in wetland",
      offset_version == offset_names_to_use3 ~ "PT moved deeper after well break. Not re-augered",
      offset_version == offset_names_to_use4 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # 4) Calculate revised_depth by subtracting the offset from sensor_depth
    revised_depth = case_when(
      !is.na(offset_value) ~ sensor_depth - offset_value,
      TRUE ~ NA_real_
    )
  ) %>% 
  filter(!is.na(offset_value))

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Note the bottomed-out depths with flag = 2
data_full <- data_full %>%
  mutate(flag = if_else(
    (Date < as.Date("2023-08-06") & revised_depth <= -0.51) |
      (Date > as.Date("2024-04-24") & revised_depth <= -0.82),
    2,
    flag
  ))

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use1, new_offset1, new_offset2,
   offset_cols_to_use1, offset_dates_to_use1,
   offset_names_to_use1, offset_names_to_use2,
   offset_vals_use1)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# L) Site: 9_439-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "9_439"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)


## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.655,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LI) Site: 9_508-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "9_508"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)


## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V3 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_3"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_3"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# run anomaly remover 
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag bottomed out data = 2
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.815,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LII) Site: 9_609-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "9_609"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)


## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------
# Run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.825,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LIII) Site: 9_77-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
site <- "9_77"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

## ------ B Explore depth versions -------------------------
data_full <- calc_stages_from_offsets(data, pivot_history)
# Select columns to plot
all_cols <- colnames(data_full)
depth_cols <- grep('depth', all_cols, value=TRUE)
not_to_plot <- c("sensor_depth")
ts_cols <- depth_cols[!depth_cols %in% not_to_plot]
print(ts_cols)

# Make plots
make_site_ts(site_ts=data_full, 
             y_vars = ts_cols, 
             qaqc_df = qaqc)
# Plot checks
checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)
# Plot offsets
all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))
quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

print(all_offsets)

# PICK OFFSET HERE
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
offset_names_to_use <- all_offset_names[all_offset_names == "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates == "P_G/L_date_1"]
offset_cols_to_use  <- c(offset_names_to_use, offset_dates_to_use)

offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
offset_vals_use <- offsets_to_use %>% select(all_of(offset_names_to_use))
new_offset <- offset_vals_use %>% unlist() %>% mean(na.rm=TRUE)

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>%
  mutate(
    offset_version = offset_names_to_use,           
    offset_value   = new_offset,            
    revised_depth  = sensor_depth - offset_value,
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

# run the anomaly remover
data_full <- anomaly_remover(data_full, 'revised_depth')

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

# Flag = 2 for bottomed out data
data_full <- data_full %>% 
  mutate(flag = if_else(
    revised_depth <= -0.535,
    2,
    flag
  )
)

data_out <- data_full %>% 
  select(
    c(
      'Site_ID', 'Date', 'sensor_depth', 'original_depth', 
      'depth_avg', 'revised_depth', 'offset_version', 'offset_value', 'flag', 'notes'
    )
  )

checks_final <- make_checks_df(data_out, qaqc)
plot_checks(checks_final, site)

## ------------ E Join Output clean up environment ------------------

# Add timeseries to output
data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

# Add checks to output
checks_final <- checks_final %>%
  mutate("Site_ID" = site) %>% 
  rename(field_check_m = chk_m,
         logger_val_m = logger_date_mean_trimmed)
output_checks <- bind_rows(output_checks, checks_final)

rm(site, data, qaqc, pivot_history, 
   status, data_full, data_out, checks_final) 

rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE OUTPUT DATA & CHECKS-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: There appears to be a baro issue creating noisy data Jan-Apr/May 2022.
# The issue is happening at both North and South baros with slightly different timeframes.
# Not sure if I can fix the problem, but let's flag it for now. 
north_ids <- c("6", "6a", "3", "7")
south_ids <- c("5", "5a", "15", "9", "14", "13", "14.9")

output_data <- output_data %>% 
  mutate(basin_id = str_split(Site_ID, '_') %>% map_chr(1)) %>% 
  mutate(
    flag = case_when(
      basin_id %in% south_ids &
        between(Date, as.Date("2022-01-27"), as.Date("2022-05-27")) ~ 4,
      basin_id %in% north_ids &
        between(Date, as.Date("2022-01-27"), as.Date("2022-04-09")) ~ 4,
      TRUE ~ flag
    )
  ) %>% 
  mutate(
    notes = case_when(
      basin_id %in% south_ids &
        between(Date, as.Date("2022-01-27"), as.Date("2022-05-27")) ~ 'Peculiarly noisy data for all wells. Baro issue?',
      basin_id %in% north_ids &
        between(Date, as.Date("2022-01-27"), as.Date("2022-04-09")) ~ 'Peculiarly noisy data for all wells. Baro issue?',
      TRUE ~ notes
    )
  ) %>% 
  select(-basin_id)

write_csv(output_checks, './data/out_data/well_checks_log_Spring2025.csv')
write_csv(output_data, './data/out_data/waterlevel_offsets_tracked_Spring2025.csv')






