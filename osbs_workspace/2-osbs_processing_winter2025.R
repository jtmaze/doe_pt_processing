# 1.0 Libraries and file paths --------------------------------------------
rm(list=ls())
library(tidyverse)
library(stringr)
source('./scripts/qaqc_functions.R')

previous_ts_path <- "D:/doe_pt_processing/data_ordway/temp/osbs_compiled_reindexed_well_depths.csv"
well_dimensions_path <- "D:/doe_pt_processing/data_ordway/temp/osbs_dimensions.csv"
well_flags_path <- "D:/doe_pt_processing/data_ordway/temp/osbs_well_timeseries_flags.csv"

raw_dir <- "D:/doe_pt_processing/data_ordway/raw_files/"
baro_dir <- "D:/doe_pt_processing/data_ordway/baro_files/"

out_dir <- "D:/doe_pt_processing/data_ordway/output/"
output_timeseries = list()

raw_files <- list.files(
  path=raw_dir,
  pattern="\\.csv",
  recursive=TRUE,
  full.names=TRUE
)

baro_files <- list.files(path=baro_dir, pattern='.csv', full.names=TRUE)

# 2.0 Concatonate the raw files and baro files  --------------------------------------------

combined_raw_files <- tibble()

for(f in raw_files){
  df <- read_csv(f, col_types = cols(`#` = col_skip()), skip=1)
  print(head(df))
  df <- df[ ,c(1, 2)]
  colnames(df)[1] <- "timestamp"
  colnames(df)[2] <- "PT"
  df$timestamp <- mdy_hms(df$timestamp)
  well_id <- strsplit(basename(f), '_LL')[[1]][1]
  well_id <- gsub("_", " ", well_id)
  df$well_id <- well_id
  
  combined_raw_files <- bind_rows(combined_raw_files, df)
  rm(df)
}

combined_baro_files <- tibble()

for(f in baro_files){
  df <- read_csv(f) %>% 
    rename(timestamp = Period,
           baro_millibar = `BP avg (mb)`) %>% 
    mutate(timestamp = dmy_hm(timestamp, tz = 'UTC')) %>% 
    select(c(timestamp, baro_millibar))
  
  combined_baro_files <- rbind(combined_baro_files, df)
  
  rm(df)
}

combined_baro_files <- combined_baro_files %>% 
  distinct(timestamp, .keep_all = TRUE)

print(unique(combined_raw_files$well_id))

# 3.0 Calculate meters of head --------------------------------------------

# ... 3.1 Merge baro data with new PT download --------------------------------------------

combined_baro_files <- combined_baro_files %>% 
  mutate(baro_psi = baro_millibar * 0.0145038) %>% 
  select(-c(baro_millibar))

combined_raw_files <- left_join(combined_raw_files, combined_baro_files)

rm(combined_baro_files)

# ... 3.2 Calculate meters of head --------------------------------------------

combined_raw_files <- combined_raw_files %>% 
  mutate(p_gauge = PT - baro_psi,
         head_m = 1000 * p_gauge / 2.2 /(2.54^2) / 100) %>% 
  select(-c(p_gauge, baro_psi, PT))

# 4.0 Convert head to well depth and combine with previous data --------------------------------------------

well_dimensions <- read_csv(well_dimensions_path)
well_flags <- read_csv(well_flags_path)

# 4.1 ... Ross Pond --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Ross Pond")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Ross Pond")
  
# 4.1.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Ross Pond"))
print(well_dimensions %>% filter(well_id == "Ross Pond"))

offset_val <- well_dimensions %>%
  filter(well_id == "Ross Pond") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

make_site_ts(raw_ts %>% rename(date=timestamp) %>% filter(flag==0), 'indexed_well_depth_m')

# NOTE: Well bottomed out in Fall 2025. Need to note the bottomed out depth and update flag info:
well_flags <- well_flags %>% 
  add_row(
    well_id = "Ross Pond", 
    flag = 2, 
    begin = as.POSIXct("2025-07-27 00:00:00", tz = "UTC"),
    end = as.POSIXct("2025-10-20 00:00:00", tz = "UTC"), 
    condition = "well_depth_m < -0.43",
    note = "Well bottomed out at new position (post Oct 2020)"
  )

raw_ts <- raw_ts %>% 
  mutate(
    flag = case_when(
      well_depth_m < -0.43 ~ 2,
      TRUE ~ flag
    ), 
    notes = case_when(
      flag == 2 ~ "Well bottomed out at new position (post Oct 2020)", 
      TRUE ~ notes
    )
  )

# 4.1.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag==0), 'indexed_well_depth_m')

output_timeseries[['Ross Pond']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.2 ... Devils Den--------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Devils Den")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Devils Den")

# 4.2.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Devils Den"))
print(well_dimensions %>% filter(well_id == "Devils Den"))

offset_val <- well_dimensions %>%
  filter(well_id == "Devils Den") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.2.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag != 3), 'indexed_well_depth_m')

output_timeseries[['Devils Den']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.3 ... Brantley North --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Brantley North")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Brantley North")

# 4.3.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Brantley North"))
print(well_dimensions %>% filter(well_id == "Brantley North"))

offset_val <- well_dimensions %>%
  filter(well_id == "Brantley North") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.3.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag == 0), 'indexed_well_depth_m')

output_timeseries[['Brantley North']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.4 ... Surprise --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Surprise")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Surprise")

# 4.4.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Surprise"))
print(well_dimensions %>% filter(well_id == "Surprise"))

offset_val <- well_dimensions %>%
  filter(well_id == "Surprise") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.4.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Surprise']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.5 ... West Ford --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "West Ford")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "West Ford")

# 4.5.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "West Ford"))
print(well_dimensions %>% filter(well_id == "West Ford"))

offset_val <- well_dimensions %>%
  filter(well_id == "West Ford") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.5.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'well_depth_m')

output_timeseries[['West Ford']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.6 ... Fish Cove --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Fish Cove")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Fish Cove")

# 4.6.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Fish Cove"))
print(well_dimensions %>% filter(well_id == "Fish Cove"))

offset_val <- well_dimensions %>%
  filter(well_id == "Fish Cove") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.6.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag == 0), 'well_depth_m')

output_timeseries[['Fish Cove']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.7 ... Anderson --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Anderson")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Anderson")

# 4.7.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Anderson"))
print(well_dimensions %>% filter(well_id == "Anderson"))

offset_val <- well_dimensions %>%
  filter(well_id == "Anderson") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.7.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'well_depth_m')

output_timeseries[['Anderson']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.8 Fox  --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Fox")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Fox")

# 4.8.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Fox"))
print(well_dimensions %>% filter(well_id == "Fox"))

offset_val <- well_dimensions %>%
  filter(well_id == "Fox") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.8.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'well_depth_m')

output_timeseries[['Fox']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.9 One Shot  --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "One Shot")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "One Shot")

# 4.9.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "One Shot"))
print(well_dimensions %>% filter(well_id == "One Shot"))

offset_val <- well_dimensions %>%
  filter(well_id == "One Shot") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.9.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag==0), 'indexed_well_depth_m')

output_timeseries[['One Shot']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.10 Harry Prarie  --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Harry Prairie")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Harry Prairie")

# 4.10.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Harry Prairie"))
print(well_dimensions %>% filter(well_id == "Harry Prairie"))

offset_val <- well_dimensions %>%
  filter(well_id == "Harry Prairie") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.10.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp) %>% filter(flag==0), 'indexed_well_depth_m')

output_timeseries[['Harry Prairie']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.11 Small  --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Small")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Small")

# 4.11.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Small"))
print(well_dimensions %>% filter(well_id == "Small"))

# !!! NOTE: This sites offset changes on June 2024
well_dimensions <- well_dimensions %>%
  # Update the end date for the second-to-last offset value
  mutate(
    end_date = if_else(
      well_id == "Small" & is.na(end_date) & version == 3,
      as.POSIXct("2024-06-10 00:00:00", tz = "UTC"),
      end_date
    )
  ) %>%
  # Update the final offset value
  add_row(
    well_id = "Small",
    version = 4,
    offset_value = -0.59, # Same as 2nd to last offset
    begin_date = as.POSIXct("2024-06-10 00:00:00", tz = "UTC"),
    end_date = NA
  )


offset_val <- well_dimensions %>%
  filter(well_id == "Small") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.11.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Small']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.12 Hansford --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Hansford")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Hansford")

# 4.12.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Hansford"))
print(well_dimensions %>% filter(well_id == "Hansford"))

offset_val <- well_dimensions %>%
  filter(well_id == "Hansford") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.12.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Hansford']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.13 Breezeway --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Breezeway")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Breezeway")

# 4.13.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Breezeway"))
print(well_dimensions %>% filter(well_id == "Breezeway"))

offset_val <- well_dimensions %>%
  filter(well_id == "Breezeway") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

make_site_ts(raw_ts %>% rename(date = timestamp), 'well_depth_m')

# NOTE: Well bottomed out in Summer/Fall 2025. Need to note the bottomed out depth and update flag info:
well_flags <- well_flags %>% 
  add_row(
    well_id = "Breezeway", 
    flag = 2, 
    begin = as.POSIXct("2025-06-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2025-10-20 00:00:00", tz = "UTC"), 
    condition = "well_depth_m < -0.61",
    note = "Well bottomed out at new position (post Oct 2020)"
  )

raw_ts <- raw_ts %>%
  mutate(
    flag = case_when(
      well_depth_m < -0.41 & timestamp >= as.POSIXct("2025-06-01 00:00:00", tz = "UTC") ~ 2,
      TRUE ~ flag
    ),
    notes = case_when(
      flag == 2 ~ "Well bottomed out at new position (post Oct 2020)",
      TRUE ~ notes
    )
  )

# 4.13.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'well_depth_m')

output_timeseries[['Breezeway']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.14 Huey --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Huey")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Huey")

# 4.14.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Huey"))
print(well_dimensions %>% filter(well_id == "Huey"))

offset_val <- well_dimensions %>%
  filter(well_id == "Huey") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.14.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Huey']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.15 Shady --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Shady")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Shady")

# 4.15.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Shady"))
print(well_dimensions %>% filter(well_id == "Shady"))

offset_val <- well_dimensions %>%
  filter(well_id == "Shady") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.15.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Shady']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.16 Blue --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Blue")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Blue")

# 4.16.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Blue"))
print(well_dimensions %>% filter(well_id == "Blue"))

# NOTE: Well discontinued in 2021 no need to calculate new data. 

# 4.16.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

output_timeseries[['Blue']] <- merged_site

rm(merged_site, raw_ts, previous_ts)

# 4.17 Brantley East --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Brantley East")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Brantley East")

# 4.17.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Brantley East"))
print(well_dimensions %>% filter(well_id == "Brantley East"))

offset_val <- well_dimensions %>%
  filter(well_id == "Brantley East") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.17.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Brantley East']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.18 Breezeway Sandhill --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Breezeway Sandhill")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Breezeway Sandhill")

# 4.18.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Breezeway Sandhill"))
print(well_dimensions %>% filter(well_id == "Breezeway Sandhill"))

offset_val <- well_dimensions %>%
  filter(well_id == "Breezeway Sandhill") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.18.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Breezeway Sandhill']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.19 Clear --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Clear")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Clear")

# 4.19.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Clear"))
print(well_dimensions %>% filter(well_id == "Clear"))

offset_val <- well_dimensions %>%
  filter(well_id == "Clear") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.19.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Clear']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.20 Enslow --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Enslow")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Enslow")

# 4.20.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Enslow"))
print(well_dimensions %>% filter(well_id == "Enslow"))

offset_val <- well_dimensions %>%
  filter(well_id == "Enslow") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.20.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Enslow']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 4.21 Gopher --------------------------------------------

raw_ts <- combined_raw_files %>% filter(well_id == "Gopher")
previous_ts <- read_csv(previous_ts_path) %>% filter(well_id == "Gopher")

# 4.21.A) ... Inspect flags calculate well depth --------------------------------------------

print(well_flags %>% filter(well_id == "Gopher"))
print(well_dimensions %>% filter(well_id == "Gopher"))

offset_val <- well_dimensions %>%
  filter(well_id == "Gopher") %>% 
  arrange(desc(begin_date)) %>% 
  slice(1) %>% 
  pull(offset_value) 

print(offset_val)

raw_ts <- raw_ts %>% 
  mutate(
    offset_value = offset_val, 
    well_depth_m = head_m + offset_val,
    indexed_well_depth_m = well_depth_m, # NOTE same as well depth since well is in recent location
    flag = 0,
    notes = NA_character_
  )

# Remove any anomalously high or low values values (e.g. download times)
raw_ts <- anomaly_remover(raw_ts, 'well_depth_m')

# 4.21.B) ... Combine the files into the output --------------------------------------------

merged_site <- bind_rows(previous_ts, raw_ts)

make_site_ts(merged_site %>% rename(date=timestamp), 'indexed_well_depth_m')

output_timeseries[['Gopher']] <- merged_site

rm(merged_site, raw_ts, previous_ts, offset_val)

# 5.0 Write the output --------------------------------------------

output <- bind_rows(output_timeseries) %>% 
  select(-c('rolling_median', 'residuals'))

ts_out_path <- paste0(out_dir, "osbs_well_depth_Fall2025")
dimensions_out_path <- paste0(out_dir, "osbs_well_dimensions_Fall2025")
flags_out_path <- paste0(out_dir, "osbs_well_flags_Fall2025")

write_csv(output, ts_out_path)
write_csv(well_dimensions, dimensions_out_path)
write_csv(well_flags, flags_out_path)


