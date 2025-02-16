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

plot_checks(make_checks_df(data_out, qaqc), site)

## ------------ E Join Output clean up environment ------------------

data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

rm(site, data, qaqc, pivot_history, status, data_full, data_out) 

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

plot_checks(make_checks_df(data_out, qaqc), site)

## ------------ E Join Output clean up environment ------------------

data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

rm(site, data, qaqc, pivot_history, status, data_full, data_out) 
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

plot_checks(make_checks_df(data_out, qaqc), site)

## ------------ E Join Output clean up environment ------------------

data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

rm(site, data, qaqc, pivot_history, status, data_full, data_out) 
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

# --------!!!!! Site missing from excel data ---------------------

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

plot_checks(make_checks_df(data_out, qaqc), site)

## ------------ E Join Output clean up environment ------------------

data_out <- data_out %>% 
  select(-c('depth_avg')) 
output_data <- bind_rows(output_data, data_out)  

rm(site, data, qaqc, pivot_history, status, data_full, data_out) 
rm(offsets_to_use, new_offset, offset_cols_to_use, 
   offset_dates_to_use, offset_names_to_use, offset_vals_use)
rm(ts_cols, not_to_plot, all_cols, all_offset_cols, all_offset_dates,
   all_offset_names, all_offsets, checks, depth_cols)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V) Site: 13_410 -------------------------------------------------------
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





