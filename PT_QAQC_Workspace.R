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
compiled_path <-'./data/compiled_stage_2.xlsx'
# Path to wetland well metadata.
# Contains field measurements for water level and well dimensions 
meta_data_path <- './data/Wetland_well_metadata_1.xlsx'
status_path <- './data/Post_Processing_Well_Status.xlsx'

# 
unique_wetland_wells <- read_excel(meta_data_path, sheet="Wetland_and_well_info") %>% 
  pull('Site_ID') %>% 
  unique()

print(unique_wetland_wells)

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
# Review with DLM and Others -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 13_267 # Switched offset when well moved
# 13_410 # Strangely low stage
# 15_4 Looks like well isn't equilabrating properly??
# 3_173 The "bottoming out" depth changed, could be and in well instead of well moving.
# 5_510 This has to be a groundwater well, or something is seriously wrong
# 9_332 No clue what's happening here. Stitched hydrograph together with made up offset on August 9th 2023

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I) Site: 13_263 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
# NOTE: Well was moved deeper use two offsets 
# - offset_v1 prior to 2022-04-14
# - offset_v2 after 2022-04-15

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
offset_names_to_use2 <- all_offset_names[all_offset_names == "offset_m_2"]
offset_dates_to_use2 <- all_offset_dates[all_offset_dates == "P_G/L_date_2"]
offset_cols_to_use2 <- c(offset_names_to_use2, offset_dates_to_use2)
offsets_to_use2 <- pivot_history %>% 
  select(all_of(offset_cols_to_use2))
offset_vals_use2 <- offsets_to_use2 %>% select(all_of(offset_names_to_use2))

new_offset1 <- offset_vals_use1 %>%  unlist() %>% mean(na.rm = TRUE)
new_offset2 <- offset_vals_use2 %>%  unlist() %>% mean(na.rm = TRUE)

data_full <- data_full %>% 
  # Apply offset #1 prior to April 14th 2022
  # Apply offset #2 after April 15th 2022
  # Delete data in between
  mutate(
    offset_version = case_when(
      Date <  as.Date("2022-04-14") ~ offset_names_to_use1,
      Date >  as.Date("2022-04-16") ~ offset_names_to_use2,
      TRUE                          ~ NA_character_  # "Between" dates get NA 
    ),
    offset_value = case_when(
      Date <  as.Date("2022-04-14") ~ new_offset1,
      Date >  as.Date("2022-04-16") ~ new_offset2,
      TRUE                          ~ NA_real_
    ),
    flag = case_when(
      Date <  as.Date("2022-04-14") ~ 1,
      Date >  as.Date("2022-04-16") ~ 0,
      TRUE                          ~ NA_real_
    ),
    notes = case_when(
      Date <  as.Date("2022-04-14") ~ "Well moved on Apr 14th 2022. Reliant on one field measurement",
      Date >  as.Date("2022-04-16") ~ NA_character_,
      TRUE                          ~ NA_character_
    ),
    revised_depth = sensor_depth - offset_value
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
   offset_vals_use1, offset_vals_use2)
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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

# - Just use offset version #1 for the entire timeseries
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
    flag           = 2,                      
    notes          = "Suspiciously low water levels in timeseries. Well is almost never inundated."           
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# site <- "13_274"
# data <- site_ts_from_xlsx_sheet(compiled_path, site)

# --------!!!!! Site 13_274 missing from excel data ---------------------

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
    flag           = 2,                      
    notes          = "Suspiciously low water levels in timeseries. Well is almost never inundated. Checks are way above"           
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# V) Site: 14/9_168-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# VI) Site: 14/9_527-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# VII) Site: 14/9_601-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# VIII) Site: 14_115-------------------------------------------------------
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
    flag           = 2,                      
    notes          = "The water level data is suspiciously low compared to the field measurements"         
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# IX) Site: 14_15-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "There's high offset uncertianity. Used V1, but it was noteably lower"         
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# X) Site: 14_418-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XI) Site: 14_500-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianity. Used V1, but much smaller than other measurements."     
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XII) Site: 14_538-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XIII) Site: 14_610-------------------------------------------------------
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
# offset_names_to_use2 <- "estimated"
# new_offset2 <- 4.42

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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XIV) Site: 14_612-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XV) Site: 14_616-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XVI) Site: 15_268-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "Limitted field measurements for QAQC. High uncertianty for offset"    
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XVII) Site: 15_4-------------------------------------------------------
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
  mutate(offset_value = new_offset,
         offset_version = offset_names_to_use) %>% 
  mutate(revised_depth = sensor_depth - offset_value) %>% 
  mutate(
    revised_depth = case_when(
      Date < as.Date("2022-10-16") ~ revised_depth, # Dates before problematic window
      Date > as.Date("2023-12-18") ~ revised_depth, # Dates after problematic window
      # Dates inside problematic window with depth > 0
      Date >= as.Date("2022-10-16") & Date <= as.Date("2023-12-18") & revised_depth > 0 ~ revised_depth,
      # Dates inside problematic window where water below ground
      TRUE ~ NA_real_
    ),
    flag = case_when(
      Date < as.Date("2022-10-16") ~ 0, # No special notes before
      Date > as.Date("2023-12-18") ~ 0, # No special notes after
      TRUE ~ 5
    ),
    notes = case_when(
      Date < as.Date("2022-10-16") ~ NA_character_, # No special notes before
      Date > as.Date("2023-12-18") ~ NA_character_, # No special notes after
      TRUE ~ "It appears this well doesn't drain filling with storm water when stage is below ground surface (Oct 2022- Dec 2023)."
    )
  )
    

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XVIII) Site: 15_409-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianty. Offset v1 ~10 cm below subsequent offsets."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XIX) Site: 15_516-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XX) Site: 3_173-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXI) Site: 3_21-------------------------------------------------------
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
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
# Offset v3 is clearly wrong
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXII) Site: 3_23-------------------------------------------------------
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
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXIII) Site: 3_244-------------------------------------------------------
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
    flag           = 0,                      
    notes          = NA_character_
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXIV) Site: 3_311-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXV) Site: 3_34-------------------------------------------------------
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
### !!!!!!! offset V1 !!!!!!!!!!!!!!!!!
# V3 offset is clearly wrong!
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXVI) Site: 3_638-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXVII) Site: 5_161-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXVIII) Site: 5_321-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXVIII) Site: 5_510-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "Wide variability in offset values. With only one solid QAQC measurement."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXIX) Site: 5_546-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
# site <- "5_546"
# data <- site_ts_from_xlsx_sheet(compiled_path, site)


# ----- !!!! Site 5_546 not in compiled wetland stage 2 !!! ----------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XXX) Site: 5_560-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "Wide variability in offset values. Sticking with offset v1"
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXI) Site: 5_573-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "Wide variability in offset values. Sticking with offset v1"
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXII) Site: 5_597 -------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXIII) Site: 5a_550 -------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXIV) Site: 5a_582 -------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXV) Site: 5a_598 -------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXVI) Site: 6_20-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXVII) Site: 6_300-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianity used V1, which was in the middle."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXVIII) Site: 6_629-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XXXIX) Site: 6_93-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianity. Used offset #1."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XL) Site: 6a_17-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianity, offset #1 much lower. Used v3 becuase it matched checks."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLI) Site: 6a_530-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLII) Site: 7_243-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLIII) Site: 7_341-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLIV) Site: 7_622-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -------- A Read the site data/metadata -----------------
# site <- "7_622"
# data <- site_ts_from_xlsx_sheet(compiled_path, site)

# ----- !!!! Site 7_622 not in compiled wetland stage 2 !!! ----------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# XLV) Site: 7_626-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLVI) Site: 9_332-------------------------------------------------------
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
offset_names_to_use2 <- "Guessed offset from TS differences"
new_offset2 <- -0.14

# Apply the chosen offset to the entire timeseries
data_full <- data_full %>% 
  # Apply offset #1 prior to April 14th 2022
  # Apply offset #2 after April 15th 2022
  # Delete data in between
  mutate(
    offset_version = case_when(
      Date <  as.Date("2023-08-07") ~ offset_names_to_use1,
      Date >  as.Date("2023-08-10") ~ offset_names_to_use2,
      TRUE                          ~ NA_character_  # "Between" dates get NA 
    ),
    offset_value = case_when(
      Date <  as.Date("2023-08-07") ~ new_offset1,
      Date >  as.Date("2023-08-10") ~ new_offset2,
      TRUE                          ~ NA_real_
    ),
    flag = 1,
    notes = "Extreme offset uncertianity used V2, then had to guess new offset. Was the well re-augered on Aug 9th 2023??",
    revised_depth = case_when(
      Date <  as.Date("2023-08-07") ~ sensor_depth - offset_value,
      Date >  as.Date("2023-08-10") ~ sensor_depth - offset_value,
      TRUE ~ NA_real_
    ),
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLVII) Site: 9_439-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLVIII) Site: 9_508-------------------------------------------------------
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
    flag           = 1,                      
    notes          = "High offset uncertianity V1 is 15cm lower than V2/V3. Used V1."
  )

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# XLIX) Site: 9_609-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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
# L) Site: 9_77-------------------------------------------------------
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

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "revised_depth"),
             qaqc)

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

write_csv(output_checks, './data/out_data/well_checks_log.csv')
write_csv(output_data, './data/out_data/waterlevel_offsets_tracked.csv')






