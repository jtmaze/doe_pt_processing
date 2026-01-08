# NOTE: About this "data tidying" script

# This is a hacky one-off script that compiles an unruly dataset which previously
# did not have enough provenance and organization. There were many conflicting versions with
# inconsistencies and data gaps between them. 

# This is my best effort to compile these disparate versions into one master version
# whenever I processed my first batch of Bradford PT downloads in Spring 2025.

# Ideally, you should never need to run this script. Just us up-to-date versions of the data

# James Maze

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(glue)
library(plotly)

raw_dir <- 'D:/doe_pt_processing/data_bradford/raw_files_F24-W25/spring_2025_batch/'
raw_files <- list.files(path=raw_dir, pattern='LL.csv', full.names=TRUE)
meta_path <- 'D:/doe_pt_processing/data_bradford/Wetland_well_metadata_JM.xlsx'
previous_data_path <- "D:/doe_pt_processing/data_bradford/compiled_stage_prior_2025_notJM.xlsx"
output_path <- "D:/doe_pt_processing/data_bradford/temp/compiled_data_to_check_offsets_spring2025.xlsx"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Compile all the latest downloads (csv files) -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_raw_files <- data.frame()

for(f in raw_files){
  df <- read_csv(f, col_types = cols(`#` = col_skip()), skip = 1)
  df <- df[ ,c(1, 2)]
  colnames(df)[1] <- "timestamp"
  colnames(df)[2] <- "PT"
  df$timestamp <- mdy_hms(df$timestamp)
  df$BasinID <- strsplit(basename(f), '_')[[1]][1]
  df$WetlandID <- strsplit(basename(f), '_')[[1]][2]
  df$well_id <- paste(df$BasinID, df$WetlandID, sep = "_")

  # add quick conditional renaming for Sunita's new wetlands
  df$well_id <- ifelse(df$well_id == "wet_wetland", "wet_wetland_east",
                       ifelse(df$well_id == "dry_wetland", "dry_wetland_west",
                              df$well_id))
  df$BasinID <- ifelse(df$BasinID == "wet", 13,
                       ifelse(df$BasinID == "dry", 13,
                              df$BasinID))
  
  combined_raw_files <- rbind(combined_raw_files, df)
  rm(df)
}

unique_wetlands <- unique(combined_raw_files$well_id)
unique_basins <- unique(combined_raw_files$BasinID)

print(unique_wetlands)
print(unique_basins)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Assign baro Loggers to the newer downloads -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Basin 14.9 has tricky syntax "/" reformat for baro matching
combined_raw_files$BasinID[
  combined_raw_files$BasinID == '14/9' | 
  combined_raw_files$BasinID == 149
] <- '14.9'

assign_baro <- function(df) {
  
  # Basins and baros key
  north_ids <- c("6", "6a", "3", "7")
  south_ids <- c("5", "5a", "15", "9", "14", "13", "14.9")
  
  # Assign baros to the dataframe
  df <- df %>% 
    mutate(BasinID = as.character(BasinID)) %>% 
    mutate(baro_region = case_when(
      BasinID %in% north_ids ~ "N",
      BasinID %in% south_ids ~ "S", 
      TRUE ~ NA_character_
    ))
  
  # Check to ensure all sites were assigned a baro
  if (any(is.na(df$baro_region))) {
    missing_ids <- unique(df$BasinID[is.na(df$baro_region)])
    print("WARNING: The Following Basins Were Not Matched to Baros")
    print(missing_ids)
  }
  #
  return(df)
}

combined_raw_files <- assign_baro(combined_raw_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1 Round the timeseries where/when PTs aren't recording on the hour -------------------------------------------------------
# Necessary to match PT values to baro values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

round_timestamps_not_on_hour <- function(df) {
  
  df$timestamp <- as.POSIXct(df$timestamp) # catch unformatted timestamps
  # Round the Date column to the nearest hour to match with baros
  df <- df %>% 
    mutate(
      rounded_time = round_date(timestamp, unit="hour"), 
      rounding_delta = as.numeric(difftime(rounded_time, timestamp, units='mins'))
    )
  df$timestamp <- df$rounded_time
  
  # Check the rounding deltas on the PTs
  # Is too high of a rounding delta problematic?
  rounding_info <- df %>% 
    filter(!is.na(PT)) %>% 
    filter(rounding_delta != 0) %>% 
    mutate(rounding_delta = round(rounding_delta)) %>% 
    select(well_id, rounding_delta) %>% 
    distinct()
  
  print(rounding_info, n=50)
  # Remove temporary columns, return the df
  df <- df %>%  select(-rounded_time, -rounding_delta)
  return(df)
}

combined_raw_files <- round_timestamps_not_on_hour(combined_raw_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.2 Join baro data with Wetland PT data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

baro_dir <- "D:/doe_pt_processing/data_bradford/"
baro_data <- read_csv(paste0(baro_dir, 'compiled_baro.csv')) %>% 
  rename(baro_region = region) %>% 
  mutate(timestamp = as.POSIXct(Date)) %>% 
  distinct() %>% # For some reason there were lots of duplicate baro observations for a given date and region
  select(-`Temp.air`, Date)


# The North baro logger was not consistently logging prior to September, 2022
# so I removed the sporadic earlier periods. 
baro_data <- baro_data %>%
  filter(!(baro_region == "N" & timestamp < as.Date("2022-09-14")))


# 1) Add rounded-to-nearest-hour timestamp
baro_hourly <- baro_data %>%
  mutate(rounded_hour = round_date(timestamp, unit = "hour")) %>% 
# 2) Hourly
  group_by(rounded_hour) %>%
  summarise(PTbaro = mean(PTbaro, na.rm = TRUE), .groups = "drop") %>% 
  rename(timestamp = rounded_hour)

# library(plotly)
# p <- plot_ly(
#   data = baro_hourly,
#   x = ~timestamp,
#   y = ~PTbaro,
#   color = ~baro_region,
#   type = "scattergl",    # WebGL for speed with large data
#   mode = "lines",
#   hovertemplate = paste(
#     "<b>%{x}</b><br>",
#     "PTbaro: %{y}<br>",
#     "Region: %{curveNumber}<extra></extra>"
#   )
# )
# p
  
combined_raw_files <- left_join(
  combined_raw_files, 
  baro_hourly, 
  by=c('timestamp'),
  relationship = 'many-to-many'
)

rm(baro_data) # Still need to keep the hourly baro data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Calculate Water Height Above Sensor -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calc_water_height <- function(df) {
  out_df <- df %>% 
    mutate(water_press = PT - PTbaro,
           head_m = 1000 * water_press / 2.2 /(2.54^2) /100)
  
  return(out_df)
}

compiled_pt_data <- calc_water_height(combined_raw_files)

rm(combined_raw_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5.0 Collate all the PG, PL and offset measurements -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wetland_well_meta <- read_excel(
  meta_path, 
  sheet = "Wetland_pivot_history"
)

# Reformat the wetland well metadata
chk1 <- wetland_well_meta %>% 
  select(Site_ID, Basin_ID, `P_G/L_date_1`, P_G_cm_1, P_L_cm_1) %>% 
  rename('PG'="P_G_cm_1", 'PL'='P_L_cm_1', 'Date'='P_G/L_date_1')

chk2 <- wetland_well_meta %>%
  select(Site_ID, Basin_ID, `P_G/L_date_2`, P_G_cm_2, P_L_cm_2) %>% 
  rename('PG'="P_G_cm_2", 'PL'='P_L_cm_2', 'Date'='P_G/L_date_2')

chk3 <- wetland_well_meta %>% 
  select(Site_ID, Basin_ID, `P_G/L_date_3`, P_G_cm_3, P_L_cm_3) %>% 
  rename('PG'="P_G_cm_3", 'PL'='P_L_cm_3', 'Date'='P_G/L_date_3')

chk4 <- wetland_well_meta %>%
  select(Site_ID, Basin_ID, `P_G/L_date_4`, P_G_cm_4, P_L_cm_4) %>% 
  rename('PG'="P_G_cm_4", 'PL'='P_L_cm_4', 'Date'='P_G/L_date_4')

chk5<-wetland_well_meta %>% 
  select(Site_ID, Basin_ID, `P_G/L_date_5`, P_G_cm_5, P_L_cm_5) %>% 
  rename('PG'="P_G_cm_5", 'PL'='P_L_cm_5', 'Date'='P_G/L_date_5')

PG_PL <- rbind(chk1, chk2, chk3, chk4, chk5) %>% 
  rename('day'='Date') %>% 
  select(Site_ID, day, PG, PL) %>% 
  rename(well_id = Site_ID)

rm(chk1, chk2, chk3, chk4, chk5, wetland_well_meta)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6.0 Dynamically apply offset overtime (NOTE BAD/OLD PROCEDURE) -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# !!! For finalized data we don't change the well's PG/PL and offset measurements
# !!! Only keeping this step for continuity of data processing
# !!! The main objective is to illustrate what data would look like with the bad/old procedure

# Join PG_PL to the spring 2025 downloads
compiled_pt_data <- compiled_pt_data %>% 
  mutate(day = as.Date(timestamp)) 
compiled_pt_data <- left_join(compiled_pt_data, PG_PL, by=c('well_id', 'day'))

compiled_pt_data <- compiled_pt_data %>% 
  arrange(well_id, timestamp) %>% 
  fill(PG, PL) 

compiled_pt_data <- compiled_pt_data %>% 
  mutate(
    # NOTE: if PG and PL aren't filled, I replace them with zero.
    # This does not matter since we no longer apply dynamic PG/PL measurements that change with time.
    PG = coalesce(as.numeric(PG), 0),
    PL = coalesce(as.numeric(PL), 0)
    ) %>% 
  mutate(well_depth_m = head_m - (PL - PG) / 100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7.0 Bind the latest downloads to the old data  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

compiled_pt_data <- compiled_pt_data %>%  
  select(timestamp, well_id, BasinID, water_press, head_m, well_depth_m, PT, PTbaro, PG, PL)

# Read the previous data
sheet_names <- excel_sheets(previous_data_path)
previous_dfs <- lapply(sheet_names, function(sheet) {
  read_excel(previous_data_path, sheet=sheet)
})
previous <- bind_rows(previous_dfs) %>% 
  rename(well_id = Site) %>% 
  mutate(timestamp = as.POSIXct(Date)) %>% 
  select(-c("Water_press", "sensor_depth", "depth", "PTbaro", "water_press"))

rm(previous_dfs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.1 Compile missing sites not in the first version of old data (pre 2024)  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There's multiple copies of previous data, so I'm stitching together missing timeframes and sites
missing_sites = c('13_274', '5_546', '14_610', '14_616') 
missing_df <- read_csv('D:/doe_pt_processing/data_bradford/archive_files/compiled_PT.csv') %>%
  mutate(WetlandID = as.character(WetlandID),
         well_id = paste(BasinID, WetlandID, sep='_'),
         timestamp = as.POSIXct(Date)) %>%
  filter(well_id %in% missing_sites) %>% 
  select(c(timestamp, well_id, PT, BasinID)) 


# Round timestamps in missing_df
missing_df <- round_timestamps_not_on_hour(missing_df)

# Add PG_PL estimates to the missing df
missing_df <- missing_df %>% mutate(day = as.Date(timestamp))
missing_df <- left_join(missing_df, PG_PL, by=c("well_id", "day")) %>% 
  select(-c(day)) %>% 
  mutate(PG = as.numeric(PG),
         PL = as.numeric(PL)) %>% 
  arrange(well_id, timestamp) %>% 
  fill(PG, PL) 

previous <- bind_rows(previous, missing_df)

rm(missing_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.2 Recalculate water depth in old data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

previous <- previous %>% 
  select(c("timestamp", "well_id", "BasinID", "PT", "PG", "PL"))

previous <- assign_baro(previous)

previous <- left_join(
  previous, 
  baro_hourly, 
  by=c('timestamp'),
  relationship = 'many-to-many'
)

previous <- calc_water_height(previous)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.3 Replace bad PT values in the old data with version Josh found -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The old data had a lot of bad values (noisy inter-day swings), Josh found a 
# better verion which he processed manually a few years ago. 
# I'm replacing those old erroneous values for hieght above sensor with Josh's version. 
# I also zoom into the plots to double check continuity between these versions.

out <- vector("list", length(unique(previous$well_id)))
names(out) <- unique(previous$well_id)

for(id in unique(previous$well_id)){
  temp <- previous %>% 
    filter(well_id == id)
  
  region <- first(temp$baro_region)
  
  # Read the data for fixing
  path <- glue("D:/doe_pt_processing/data_bradford/manually_processed_data/{id}_WD.xlsx")
  # Quick check to see if the site exists
  if (!file.exists(path)) {
    message(glue("Site {id}: SKIP — file not found: {path}"))
    out[[id]] <- temp
    next
  }
  backfill <- read_excel(path, sheet=id) %>% 
    select(c(`Date Time`, `Meters head`)) %>% 
    mutate(timestamp = as.POSIXct(`Date Time`)) %>% 
    rename(head_m = `Meters head`) %>% 
    select(c("timestamp", "head_m"))
    
  # Swap ranges corresponding to baro issues
  swap_range <- if (identical(region, "S")) {
    as.Date(c("2022-01-27", "2022-04-15"))
  } else {
    as.Date(c("2022-02-08", "2022-03-29"))
  }
  
  message(glue("Site {id}: applying backfill between {swap_range[1]} and {swap_range[2]}"))
  
  # Replace wonky old values from Josh's better version
  temp <- temp %>%
    left_join(backfill %>% rename(head_m_bf = head_m), by = "timestamp") %>%
    mutate(
      swap_mask = dplyr::between(as.Date(timestamp), swap_range[1], swap_range[2]) &
        !is.na(head_m_bf),
      head_m = if_else(swap_mask, head_m_bf, head_m)
    ) %>%
    select(-head_m_bf, -swap_mask)
  
  out[[id]] <- temp
}

fixed <- bind_rows(out)

# Quick diagnostic plot to compare between versions. 
id <-  '14_610'
temp <- fixed %>% filter(well_id == id)

p <- plot_ly(
  temp,
  x = ~timestamp,
  y = ~head_m,
  type = "scatter",
  mode = "lines"
) %>%
  plotly::layout(
    title = glue("Sensor Depth for {id}"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "Sensor depth (m)")
  )

p
rm(p, temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.4 Use the (probably incorrect) PG PL values to get well depth. We fix this later -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fixed <- fixed %>% 
  mutate(
    well_depth_m = head_m - (PL - PG) / 100
  ) %>% 
  select(-baro_region) 


output <- bind_rows(compiled_pt_data, fixed) 

id <-  '3_173'
temp <- output %>%  filter(well_id == id)

p <- plot_ly(
  temp,
  x = ~timestamp,
  y = ~well_depth_m,
  type = "scatter",
  mode = "lines"
) %>%
  plotly::layout(
    title = glue("Sensor Depth for {id}"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "Sensor depth (m)")
  )

p

rm(temp, p)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8.0 Write the compiled data  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sheet_list <- split(output, output$well_id)
write.xlsx(
  sheet_list, 
  output_path
)


