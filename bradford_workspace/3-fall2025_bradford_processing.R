# 1.0 Libraries and file paths --------------------------------------------

rm(list=ls())

library(tidyverse)
library(plotly)
source('./scripts/QAQC_functions.R')

previous_ts_path <- "D:/doe_pt_processing/data_bradford/output_data/bradford_stage_Spring2025_offsets_tracked.csv"
old_checks_path <- "D:/doe_pt_processing/data_bradford/output_data/bradford_well_checks_Spring2025.csv"
new_checks_path <- "D:/doe_pt_processing/data_bradford/raw_files/Field_Notes_Dec2025.xlsx"
raw_dir <- "D:/doe_pt_processing/data_bradford/raw_files/fall_2025_batch/"
baro_dir <- "D:/doe_pt_processing/data_bradford/baro_data/compiled_baro_to_Dec2025.csv"

out_dir <- "D:/doe_pt_processing/data_bradford/output_data/"
output_timeseries <- list()
output_checks <- list()
output_flags <- list()

raw_files <- list.files(
  path=raw_dir,
  pattern="\\.csv",
  recursive=TRUE,
  full.names=TRUE
)


# 2.0 Concatonate the raw files ------------------------------------------

combined_raw_files <- tibble()

for (f in raw_files){
  df <- read_csv(f, col_types = cols(`#` = col_skip()), skip = 1)
  df <- df[ ,c(1, 2)]
  colnames(df)[1] <- "timestamp"
  colnames(df)[2] <- "PT"
  df$timestamp <- mdy_hms(df$timestamp)
  
  # Extract the wetland and the basin info from file path
  well_id <- str_split(basename(f), "_LL")[[1]][1]
  df$well_id <- well_id
  basin_id <- str_split(well_id, "_")[[1]][1]
  wetland_id <- str_split(well_id, "_")[[1]][2]
  
  # if statement to catch Sunita's unique wetland ids
  if (well_id == "dry_wetland_west" || well_id == "wet_wetland_east"){
    basin_id = 13
    wetland_id = well_id
  }
  combined_raw_files <- bind_rows(combined_raw_files, df)
  
  rm(df, f, basin_id, well_id)
}

unique_wetlands <- unique(combined_raw_files$well_id)

print(unique_wetlands)
rm(raw_dir)

# 3.0 Assign baro data and calculate head_m ----------------------------------------------------

# ... 3.1 Assign baro ------------------------------------------------------------

# Read the baro data and round to the nearest hour. 
baro_data <- read_csv(baro_dir) %>% 
  filter(Date >= as.POSIXct('2024-01-01')) %>% 
  mutate(rounded_hour = round_date(Date, unit = "hour")) %>% 
  group_by(rounded_hour) %>%
  summarise(PTbaro = mean(PTbaro, na.rm = TRUE), .groups = "drop") %>% 
  rename(timestamp = rounded_hour)

# Read the FAWN baro data to gap-fill missing baro section
fawn <- read_csv(
  "D:/doe_pt_processing/data_bradford/baro_data/FAWN_alachua_baro_gapfill_Sep2025.csv"
) %>% 
  mutate(
    Period = mdy_hm(Period),
    timestamp = round_date(Period, "hour"),
    PTbaro = `BP avg (mb)` * 0.0145038 #Convert millibar to PSI
  ) %>%
  select(c(timestamp, PTbaro))

baro_data <- bind_rows(baro_data, fawn) %>% arrange(timestamp) %>% 
  group_by(timestamp) %>% 
  summarise(PTbaro = mean(PTbaro, na.rm = TRUE), .groups = "drop")

# plot_ly(
#   data = baro_data,
#   x = ~timestamp,
#   y = ~PTbaro,
#   type = "scatter",
#   mode = "lines"
# ) %>%
#   layout(
#     xaxis = list(title = "Timestamp"),
#     yaxis = list(title = "PTbaro"),
#     title = "Barometric Pressure Time Series"
#   )

combined_raw_files <- left_join(
  combined_raw_files, 
  baro_data, 
  by=c('timestamp')
)

rm(baro_data, baro_dir, fawn)

# ... 3.2 Calculate meters of head

combined_raw_files <- combined_raw_files %>% 
  mutate(p_gauge = PT - PTbaro,
         head_m = 1000 * p_gauge / 2.2 / (2.54^2) / 100) %>% 
  select(-c('PTbaro', 'p_gauge', 'PT'))

# 4.0 Check the field data and well offset ----------------------------------------------------

previous <- read_csv(previous_ts_path) %>% 
  select(-c(original_depth_m))

old_checks <- read_csv(old_checks_path) %>% 
  filter(version == 'well_depth_m') %>% 
  select(-c('version'))

new_checks <- read_xlsx(new_checks_path) %>% 
  select(-c('logger_volts'))

rm(previous_ts_path, old_checks_path, new_checks_path)

# 4.1 ... 13_263 ----------------------------------------------------

site <- '13_263'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.1.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)

latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 
print(latest_offset)

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 1,
    notes = "Well's bottoming-out depth is not stable"
  )

rm(offset_history, latest_offset)

# 4.1.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')
 
site_checks <- site_checks %>% 
 add_row(
   diff = new_site_checks$diff,
   field_check_m = new_site_checks$chk_m,
   logger_val_m = new_site_checks$logger_date_mean_trimmed,
   well_id = site, 
   date = new_site_checks$date
 )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
 mutate(
   flag = case_when(
       timestamp >= as.POSIXct("2024-12-03 00:00:00") & well_depth_m <= -0.57 ~ 2,
       TRUE ~ flag),
   notes = case_when(
     timestamp >= as.POSIXct("2024-12-03 00:00:00") & well_depth_m <= -0.57 ~ "Well bottomed out, but min depth is changing.",
     TRUE ~ notes),
)

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.1.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.2 ... 13_267 ----------------------------------------------------

site <- '13_267'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.2.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.2.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.94 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.94 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.2.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.3 ... 13_271 ----------------------------------------------------

site <- '13_271'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.3.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.3.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )


merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.66 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.66 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.3.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.4 ... 13_274 ----------------------------------------------------

site <- '13_274'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.4.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.4.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.85 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.85 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)
rm(new_site_checks)

# 4.4.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.5 ... 13_410 ----------------------------------------------------

site <- '13_410'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.5.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.5.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.65 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.65 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.5.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.6 ... 14.9_168 ----------------------------------------------------

site <- '14.9_168'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == "14/9_168") %>% 
  mutate(well_id = site)
site_checks <- old_checks %>% filter(well_id == "14/9_168") %>% 
  mutate(well_id = site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.6.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.6.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2025-05-23 00:00:00") ~ 1,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2025-05-23 00:00:00") ~ "Well was broken. Could not determine breaking date. Uncertian data.",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.6.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.7 ... 14.9_527  ----------------------------------------------------

site <- '14.9_527'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == "14/9_527") %>% 
  mutate(well_id = site)
site_checks <- old_checks %>% filter(well_id == "14/9_527") %>% 
  mutate(well_id = site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.7.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.7 B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.73 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.73 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.7.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.8 ... 14.9_601  ----------------------------------------------------

site <- '14.9_601'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == "14/9_601") %>% 
  mutate(well_id = site)
site_checks <- old_checks %>% filter(well_id == "14/9_601") %>% 
  mutate(well_id = site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.8.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.8.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.8.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.9 ... 14_115 ----------------------------------------------------

site <- '14_115'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.9.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.9.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.405 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.405 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.9.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.10 ... 14_15 ----------------------------------------------------

site <- '14_15'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.10.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.10.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.32 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.32 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.10.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.11 ... 14_418 ----------------------------------------------------

site <- '14_418'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.11.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 
print(latest_offset)

temporary_offset <- NA_real_ # well knocked over during logging
temporary_offset_name <- "well_knocked_over"

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = case_when(
      timestamp <= as.POSIXct('2025-08-05 00:00:00') ~ latest_offset$value,
      TRUE ~ temporary_offset
    ),
    offset_version = case_when(
      timestamp <= as.POSIXct('2025-08-05 00:00:00') ~ latest_offset$offset_version,
      TRUE ~ temporary_offset_name
    ),
    well_depth_m = head_m - offset_value,
    flag = case_when(
      timestamp <= as.POSIXct('2025-08-05 00:00:00') ~ 0,
      TRUE ~ 1
    ),
    notes = case_when(
      timestamp <= as.POSIXct('2025-08-05 00:00:00') ~ NA_character_,
      TRUE ~ "Well knocked over, but PT still logging (Aug 5, 2025). Data to unreliable to fix."
    )
  )

rm(offset_history, latest_offset, temporary_offset, temporary_offset_name)

# 4.11.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.11.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.12 ... 14_500 ----------------------------------------------------

site <- '14_500'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.12.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.12.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.50 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.50 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.12.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.13 ... 14_538 ----------------------------------------------------

site <- '14_538'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.13.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.13.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts) %>% 
  filter(
    (timestamp <= as.POSIXct('2024-12-08 00:00:00') | 
       timestamp >= as.POSIXct("2024-12-10 00:00:00"))
  )

make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.13.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.14 ... 14_610  ----------------------------------------------------

site <- '14_610'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.14.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.14.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.565 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.565 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.14.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.15 ... 14_612  ----------------------------------------------------

site <- '14_612'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.15.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.15.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts) %>% 
  # Remove bad data on download date
  filter(
    (timestamp <= as.POSIXct('2024-12-09 00:00:00') | 
       timestamp >= as.POSIXct("2024-12-11 00:00:00"))
  )

make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.70 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.70 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.15.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.16 ... 14_616 ----------------------------------------------------

site <- '14_616'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.16.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.16.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2024-04-22") ~ 1,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2024-04-22") ~ "Well broken, not confident in timeseries. Could not discern break date.",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.16.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.17 ... 15_268 ----------------------------------------------------

site <- '15_268'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.17.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.17.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.59 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.59 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.17.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.18 ... 15_4 ----------------------------------------------------

site <- '15_4'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.18.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.18.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.65 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.65 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.18.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.19 ... 15_409 ----------------------------------------------------

site <- '15_409'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.19.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

new_offset <- 0.37
new_offset_name <- "rehung_Oct2025"

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = case_when(
      timestamp <= as.POSIXct('2025-10-14 00:00:00') ~ latest_offset$value,
      TRUE ~ new_offset
    ),
    offset_version = case_when(
      timestamp <= as.POSIXct('2025-12-02 00:00:00') ~ latest_offset$offset_version,
      TRUE ~ new_offset_name
    ),
    well_depth_m = head_m - offset_value,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset, new_offset, new_offset_name)

# 4.19.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.52 & timestamp <= as.POSIXct('2025-10-14 00:00:00') ~ 2,
      well_depth_m <= -0.38 & timestamp > as.POSIXct('2025-10-14 00:00:00') ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.52 & timestamp <= as.POSIXct('2025-10-14 00:00:00') ~ "Well bottomed out",
      well_depth_m <= -0.38  & timestamp > as.POSIXct('2025-10-14 00:00:00') ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.19.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.20 ... 15_516  ----------------------------------------------------

site <- '15_516'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.20.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 1,
    notes = "Bottoming out depth is increasing (i.e. well moving)"
  )

rm(offset_history, latest_offset)

# 4.20.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

# TODO: Figure out how/why this well's bottom-out depth is gradually dropping. 
merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2025-01-01 00:00:00") & well_depth_m <= -0.61 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2025-01-01 00:00:00") & well_depth_m <= -0.61 ~ "Well bottomed out, but min depth is changing.",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.20.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.21 ... 3_173  ----------------------------------------------------

site <- '3_173'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.21.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.21.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.47 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.47 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.21.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.22 ... 3_21 ----------------------------------------------------

site <- '3_21'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.22.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.22.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

flag_summary <- flag_history_from_previous(previous_ts) 
print(flag_summary)

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.77 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.77 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.22.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.23 ... 3_23 ----------------------------------------------------

site <- '3_23'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.23.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.23.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

# TODO: Update flagging once I see Dec 2025 field check
merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.10 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.10 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)
rm(new_site_checks)

# 4.23.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.24 ... 3_244 ----------------------------------------------------

# NOTE: Unable to find the well durring the latest download.

site <- '3_244'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.24.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.24.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.24.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.25 ... 3_311 ----------------------------------------------------

site <- '3_311'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.25.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.25.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.66 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.66 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts) 
print(flag_summary)

rm(new_site_checks)

# 4.25.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.26 ... 3_34 ----------------------------------------------------

site <- '3_34'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.26.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 
print(latest_offset)

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 1,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.26.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2025-02-25 00:00:00") & well_depth_m <= -0.40 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2025-02-25 00:00:00") & well_depth_m <= -0.40 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.26.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.27 ... 3_638 ----------------------------------------------------

site <- '3_638'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.27.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.27.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2025-02-25 00:00:00") & well_depth_m <= -0.52 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2025-02-25 00:00:00") & well_depth_m <= -0.52 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.27.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.28 ... 5_161 ----------------------------------------------------

site <- '5_161'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.28.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.28.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

flag_summary <- flag_history_from_previous(previous_ts) 
print(flag_summary)

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.70 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.70 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.28.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.29 ... 5_321 ----------------------------------------------------

site <- '5_321'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.29.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.29.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.02 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.02 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.29.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.30 ... 5_510 ----------------------------------------------------

site <- '5_510'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.30.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.30.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.57 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.57 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.30.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.31 ... 5_546 ----------------------------------------------------

site <- '5_546'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.31.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.31.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.05 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.05 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.31.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.32 ... 5_560 ----------------------------------------------------

site <- '5_560'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.32.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.32.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.68 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.68 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.32.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.33 ... 5_573 ----------------------------------------------------

site <- '5_573'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.33.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.33.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.68 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.68 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.33.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.34 ... 5_597 ----------------------------------------------------

site <- '5_597'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.34.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.34.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.34.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.35 ... 5a_550 ----------------------------------------------------

site <- '5a_550'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.35.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.35.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.25 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.25 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.35.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.36 ... 5a_582 ----------------------------------------------------

site <- '5a_582'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.36.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.36.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.34 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.34 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.36.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.37 ... 5a_598 ----------------------------------------------------

# NOTE: Logger did not relaunch in November of 2024
site <- '5a_598'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.37.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.37.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.37.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.38 ... 6_20 ----------------------------------------------------

site <- '6_20'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.38.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.38.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.42 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.42 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.38.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.39 ... 6_300 ----------------------------------------------------

site <- '6_300'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.39.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.39.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.85 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.85 ~ "Well bottomed out",
      TRUE ~ notes),
  )


flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.39.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.40 ... 6_629 ----------------------------------------------------

site <- '6_629'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.40.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.40.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.19 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.19 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.40.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.41 ... 6_93 ----------------------------------------------------

site <- '6_93'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.41.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.41.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.21 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.21 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.41.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.42 ... 6a_17 ----------------------------------------------------

site <- '6a_17'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.42.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.42.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.42.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.43 ... 6a_530 ----------------------------------------------------

site <- '6a_530'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.43.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.43.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.69 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.69 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.43.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.44 ...  7_243----------------------------------------------------

site <- '7_243'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.44.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.44.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.72 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.72 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.44.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.45 ... 7_341 ----------------------------------------------------

site <- '7_341'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.45.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.45.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -1.84 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -1.84 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.45.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.46 ... 7_622 ----------------------------------------------------

site <- '7_622'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.46.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.46.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.31 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.31 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.46.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.47 ... 7_626 ----------------------------------------------------

site <- '7_626'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.47.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.47.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.50 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.50 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.47.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.48 ... 9_332 ----------------------------------------------------

site <- '9_332'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.48.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.48.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts %>% filter(flag != 1), 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2024-12-02 00:00:00") & well_depth_m <= -0.80 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      timestamp >= as.POSIXct("2024-12-02 00:00:00") & well_depth_m <= -0.80 ~ "Well bottomed out.",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.48.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.49 ... 9_439 ----------------------------------------------------

site <- '9_439'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.49.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.49.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.64 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.64 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.49.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.50 ... 9_508 ----------------------------------------------------

site <- '9_508'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.50.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.50.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.78 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.78 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.50.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.51 ...  9_609 ----------------------------------------------------

site <- '9_609'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.51.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.51.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.82 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.82 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.51.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.52 ...  9_77 ----------------------------------------------------

site <- '9_77'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.52.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.52.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

new_site_checks <- calculate_chk_ts_diffs(
  ts=raw_ts, 
  qaqc= new_site_checks %>% rename(meter = field_check_m),
  version='well_depth_m')

site_checks <- site_checks %>% 
  add_row(
    diff = new_site_checks$diff,
    field_check_m = new_site_checks$chk_m,
    logger_val_m = new_site_checks$logger_date_mean_trimmed,
    well_id = site, 
    date = new_site_checks$date
  )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.525 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.525 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.52.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.53 ... Sunita's Eastern Wetland (wet) ----------------------------------------------------

site <- 'wet_wetland_east'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.53.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.53.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

# new_site_checks <- calculate_chk_ts_diffs(
#   ts=raw_ts, 
#   qaqc= new_site_checks %>% rename(meter = field_check_m),
#   version='well_depth_m')

# site_checks <- site_checks %>% 
#   add_row(
#     diff = new_site_checks$diff,
#     field_check_m = new_site_checks$chk_m,
#     logger_val_m = new_site_checks$logger_date_mean_trimmed,
#     well_id = site, 
#     date = new_site_checks$date
#   )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.68 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.68 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.53.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 4.54 ... Sunita's Western Wetland (dry) ----------------------------------------------------

site <- 'dry_wetland_west'

raw_ts <- combined_raw_files %>% filter(well_id == site)
previous_ts <- previous %>% filter(well_id == site)
site_checks <- old_checks %>% filter(well_id == site)
new_site_checks <- new_checks %>% filter(well_id == site)

# 4.54.A) ... Inspect raw data calculate well depth --------------------------------------------

offset_history <- offset_history_from_previous(previous_ts)
print(offset_history)
latest_offset <- offset_history %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) 

raw_ts <- raw_ts %>% 
  mutate(
    well_depth_m = head_m - latest_offset$value,
    offset_value = latest_offset$value,
    offset_version = latest_offset$offset_version,
    flag = 0,
    notes = NA_character_
  )

rm(offset_history, latest_offset)

# 4.54.B) ... Update/Investigate the well's flags, notes, and checks ---------------------------

# new_site_checks <- calculate_chk_ts_diffs(
#   ts=raw_ts, 
#   qaqc= new_site_checks %>% rename(meter = field_check_m),
#   version='well_depth_m')

# site_checks <- site_checks %>% 
#   add_row(
#     diff = new_site_checks$diff,
#     field_check_m = new_site_checks$chk_m,
#     logger_val_m = new_site_checks$logger_date_mean_trimmed,
#     well_id = site, 
#     date = new_site_checks$date
#   )

merged_ts <- bind_rows(previous_ts, raw_ts)
make_site_ts(merged_ts, 'well_depth_m', site_checks %>% rename(meter = field_check_m))

merged_ts <- merged_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m <= -0.68 ~ 2,
      TRUE ~ flag),
    notes = case_when(
      well_depth_m <= -0.68 ~ "Well bottomed out",
      TRUE ~ notes),
  )

flag_summary <- flag_history_from_previous(merged_ts)
print(flag_summary)

rm(new_site_checks)

# 4.54.C) ... Combine the files into the output --------------------------------------------

output_timeseries[[site]] <- merged_ts
output_checks[[site]] <- site_checks
output_flags[[site]] <- flag_summary

rm(raw_ts, previous_ts, merged_ts, site_checks, flag_summary)

# 5.0 Write the output ----------------------------------------------------

output <- bind_rows(output_timeseries)
checks <- bind_rows(output_checks)
flags <- bind_rows(output_flags)

output <- output %>% 
  mutate(
    flag = case_when(
      timestamp >= as.POSIXct("2025-08-29") & 
        timestamp <= as.POSIXct("2025-09-26") & 
        flag == 0 ~ 4,
      TRUE ~ flag
    ),
    notes = case_when(
      flag == 4 ~ "Missing baro data needed to gap-fill from FAWN data.",
      TRUE ~ notes
    )
  )

out_ts_path <- paste0(out_dir, "bradford_stage_Winter2025_offsets_tracked.csv")
out_checks_path <- paste0(out_dir, "bradford_stage_Winter2025_checks.csv")
out_flags_path <- paste0(out_dir, "bradford_stage_Winter2025_flags.csv")

write_csv(output, out_ts_path)
write_csv(checks, out_checks_path)
write_csv(flags, out_flags_path)

