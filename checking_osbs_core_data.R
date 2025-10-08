# This scripts purpose is to compile and view the OSBS stage data sent by Josh E.
# Essentially, I rename columns to avoid special cases, check the time series, and 
# concatenate everything into .csv instead of an excel workbook. 

# 1.0 Libraries and file paths --------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
source('./scripts/qaqc_functions.R')

stage_dir <- "D:/doe_pt_processing/data_ordway/uncleaned_osbs_core_wells_2018_2025.xlsx"

# 2.0 Data read and cleaning ----------------------------------------------

select_clean_cols <- function(df, site_name) {
  temp <- df %>%
    select(
      Date_Time,
      `Meter of Head`,
      `Well Water depth (m)`,
      `Max Depth (m)`,
      `Max Depth (m) Removed Bottomed Out Records`
    ) %>%
    rename(
      Date = Date_Time,
      head_m = `Meter of Head`,
      well_depth_m = `Well Water depth (m)`,
      max_depth_m = `Max Depth (m)`,
      max_depth_cleaned_m = `Max Depth (m) Removed Bottomed Out Records`
    ) %>%
    mutate(
      well_id = site_name,
      offset_val = round(head_m - well_depth_m, digits = 3),
      flag = NA_integer_
    )
  
  return(temp)
}


find_offset_begin_end <- function(stage_df, offset_df, well_id_filter) {
  
  # Checks the difference between head above PT and water depth relative to 
  # ground surface. Essentially, I want to ensure we are not arbitrarily changing
  # the well datum. 
  
  ranges <- stage_df %>%
    filter(!is.na(offset_val)) %>% 
    group_by(offset_val) %>% 
    summarize(
      begin_date = min(Date, na.rm = TRUE),
      end_date = max(Date, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print(ranges)
  
  new_offset_df <- offset_df %>% 
    filter(well_id == well_id_filter) %>% 
    left_join(ranges, by = 'offset_val')
  
  return(new_offset_df)
  
}

#...2.1 Ross Pond -------------------------------------------------------

# DATA PROCESSING NOTES:
# - Logger malfunction due to HOBO shuttle issue between 5/23/19 to 7/26/19. Data not retrievable

# - Level logger bottoms out between September 2019 and October 2020 which prompted deeper well installation on 10/6/2020

# - Well without a logger from 6/16/21 to 3/9/22 due to logger shortage in the lab (Onset level logger backorder)

# - Conductivity logger installed 10/31/2023


ross_pond <- read_xlsx(
  stage_dir,
  sheet = 'Ross'
)

ross_pond <- select_clean_cols(ross_pond, 'Ross Pond')
tz = attr(ross_pond$Date, 'tzone')
make_site_ts(ross_pond, y_var='max_depth_m')


# Clean final few data points
ross_pond <- ross_pond %>% 
  filter(
    Date <= as.POSIXct('2020-10-6 12:00:00', tz = 'UTC') | 
      Date >= as.POSIXct('2020-10-6 15:00:00', tz = 'UTC')
  )

offset_val <- unique(ross_pond$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Ross Pond', 
  stringsAsFactors = FALSE
)

offsets_ross_pond <- find_offset_begin_end(ross_pond, offsets_temp, 'Ross Pond')

# Add flags to the data

ross_pond <- ross_pond %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

# KEEP high confidence data:
ross_pond_clean <- ross_pond %>% 
  filter(Date >= as.POSIXct('2022-3-9 12:00:00')) %>% 
  select(c('Date', 'well_id', 'well_depth_m', 'max_depth_m', 'offset_val', 'flag'))

make_site_ts(ross_pond_clean, 'well_depth_m')

#...2.2 Devils Den -------------------------------------------------------

# DATA PROCESSING NOTES:
# - Level logger bottomed out from March 2020 to October 2020. 
# Well moved to a deeper location on 10/16/2020

# - Well was too deep to access to 3/28/25

devils_den <- read_xlsx(
  stage_dir,
  sheet = 'Devils Den'
)

devils_den <- select_clean_cols(devils_den, 'Devils Den')
tz = attr(devils_den$Date, 'tzone')
make_site_ts(devils_den, y_var='max_depth_m')

offset_val <- unique(devils_den$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Devils Den', 
  stringsAsFactors = FALSE
)

offsets_devils_den <- find_offset_begin_end(devils_den, offsets_temp, 'Devils Den')

# Add flags to the data

devils_den <- devils_den %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

devils_den_clean <- devils_den %>% 
  filter(Date >= as.POSIXct('2020-10-06 14:00:00')) %>% 
  select(c('Date', 'well_id', 'well_depth_m', 'max_depth_m', 'offset_val', 'flag'))

make_site_ts(devils_den_clean, y_vars = 'well_depth_m')
  

#...2.3 Brantley North -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Negative (anamolous) values observed on 1/7/20, 1/8/20 and 1/9/20. 
# They were removed from time-series (level logger malfunction). 

# - 3/19/20 to 10/7/20 level logger bottomed out prompting deeper installation on 10/7/2020

# - 3/3/22: The tape adhering the logger to the well (to allow it to suspend) was ripped. 
# The logger was resting on the well bottom. Reattached tape and the new L to P is 128 cm. 
# This has not yet been applied to the time series (both the new L to P and
# finding the exact time when the logger string snapped). 

brantley_north <- read_xlsx(
  stage_dir,
  sheet = 'Brantley North'
)

brantley_north <- select_clean_cols(brantley_north, 'Brantley North')
tz = attr(brantley_north$Date, 'tzone')
make_site_ts(brantley_north, y_var='max_depth_m')


offset_val <- unique(brantley_north$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Brantley North', 
  stringsAsFactors = FALSE
)

offsets_brantly_north <- find_offset_begin_end(brantley_north, offsets_temp, 'Brantley North')

# Add flags to the data

brantley_north <- brantley_north %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

# NOTE adjusting later depth based on site notes from Josh
p_g <- 0.48
p_l <- 1.28
offset <- p_l - p_g
well_to_lowest <- 25.65 - 25.48

brantley_north_clean <- brantley_north %>% 
  filter(Date >= as.POSIXct('2022-03-03 12:00:00')) %>% 
  select(c('Date', 'well_id', 'head_m', 'offset_val', 'flag')) %>% 
  mutate(
    well_depth_m = head_m - offset,
    max_depth_m = well_depth_m + well_to_lowest
  ) %>% 
  select(-c('head_m'))

make_site_ts(brantley_north_clean, 'max_depth_m')

#...2.4 Surprise -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Logger malfunction due to HOBO shuttle issue between 6/3/19 to 8/7/19. Data not retrievable

# - Well was without a logger from 6/16/21 until 3/3/22 due to logger shortage in lab (Onset level logger backorder)

# - Well disturbed (crooked but still in place) but logger string was snapped 
# upon arrival during the Oct 2023 visit (possilble gator/animal activity). 
# The well had to be pulled to retrieve the level logger at the bottom of the well. 
# The well was reinstalled in a new location closer to shore (current location)

surprise <- read_xlsx(
  stage_dir,
  sheet = 'Surprise'
)

surprise <- select_clean_cols(surprise, 'Surprise Pond')
tz = attr(surprise$Date, 'tzone')
make_site_ts(surprise, y_var='max_depth_m')

offset_val <- unique(surprise$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Surprise Pond', 
  stringsAsFactors = FALSE
)

offsets_surprise <- find_offset_begin_end(surprise, offsets_temp, 'Surprise Pond')

# Add flags to the data

surprise <- surprise %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

# Had to adjust offset values to keep well datum consistent after the gator damage.
offset_June24_curr <- 0.61
offset_Oct23_Jun24 <- 0.65
offset_Sept23_Oct23 <- 1.25
offset_Mar22_Sep23 <- 1.15

surprise_clean <- surprise %>% 
  filter(Date >= as.POSIXct('2022-3-3 14:00:00')) %>% 
  select(c('Date', 'well_id', 'head_m', 'max_depth_m', 'offset_val', 'flag')) %>% 
  mutate(
    well_depth_m = case_when(
      Date > as.POSIXct('2024-06-10 16:00:00', tz='UTC') ~ head_m - offset_June24_curr,
      Date < as.POSIXct('2024-06-10 1:00:00', tz='UTC') & Date > as.POSIXct('2023-10-11 15:00:00', tz='UTC') ~ head_m - offset_Oct23_Jun24,
      Date < as.POSIXct('2023-10-11 10:00:00', tz='UTC') & Date > as.POSIXct('2023-09-01 22:00:00', tz='UTC') ~ head_m - offset_Sept23_Oct23,
      Date < as.POSIXct('2023-09-01 8:00:00', tz='UTC') ~ head_m - offset_Mar22_Sep23
    )
  )

make_site_ts(surprise_clean, 'well_depth_m')
  
rm(offset_June24_curr, offset_Oct23_Jun24, offset_Sept23_Oct23, offset_Mar22_Sep23)  

#...2.5 West Ford -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Logger malfunction due to HOBO shuttle issue between 5/23/19 to 7/26/19. Data not retrievable

# - Level logger bottomed out between Sept 2019 and Oct 2020 (no meaningful data obtained from the 1/16/2020 well install)

# - Well Installed deeper 10/6/2020 to prevent level logger from bottoming out.


west_ford <- read_xlsx(
  stage_dir,
  sheet = 'West Ford'
)

west_ford <- select_clean_cols(west_ford, 'West Ford')
tz = attr(west_ford$Date, 'tzone')
make_site_ts(west_ford, y_var='max_depth_m')

offset_val <- unique(west_ford$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='West Ford', 
  stringsAsFactors = FALSE
)

offsets_west_ford <- find_offset_begin_end(west_ford, offsets_temp, 'West Ford')

# Add flags to the data

west_ford <- west_ford %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

west_ford_clean <- west_ford %>% 
  filter(Date >= as.POSIXct('2020-10-6 17:00:00')) %>% 
  select(c('Date', 'well_id', 'well_depth_m', 'max_depth_m', 'offset_val', 'flag'))
  
make_site_ts(west_ford_clean, 'well_depth_m')

#...2.6 Fish Cove -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Well was moved shallower on 6/3/19 for easier access during high stage 
# (well was previously not accessible for months after initial install)

# - Logger battery died 1/16/20 and replaced with a new logger that later appeared to be 
# malfunctoning but we did not have another replacement logger until 3/2/22.
# Well was without a logger from 6/16/21 to 3/2/22 due to logger defecit in lab (Onset backorder on loggers). 

# - Old well found knocked over (possible gator activity) so the well was reinstalled when new loggers were received from Onset on 3/2/22 (current location)

# - Conductivity logger installed 10/31/2023

fish_cove <- read_xlsx(
  stage_dir,
  sheet = 'Fish Cove'
)

fish_cove <- select_clean_cols(fish_cove, 'Fishcove')
tz = attr(fish_cove$Date, 'tzone')
make_site_ts(fish_cove, y_var='max_depth_m')


offset_val <- unique(fish_cove$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Fishcove', 
  stringsAsFactors = FALSE
)

offsets_fish_cove <- find_offset_begin_end(fish_cove, offsets_temp, 'Fishcove')

# Add flags to the data

fish_cove <- fish_cove %>% 
  mutate(
    flag = case_when(
      is.na(max_depth_m) ~ 5,
      is.na(max_depth_cleaned_m) & !is.na(max_depth_m) ~ 2,
      TRUE ~ 0
    )
  )

fish_cove_clean <- fish_cove %>% 
  filter(Date >= as.POSIXct('2022-03-03 12:00:00')) %>% 
  select(c('Date', 'well_id', 'well_depth_m', 'max_depth_m', 'offset_val', 'flag'))

make_site_ts(fish_cove_clean, 'well_depth_m')

#3.0 Compile the cleaned data -------------------------------------------------------

output <- bind_rows(
  ross_pond_clean,
  brantley_north_clean,
  devils_den_clean,
  surprise_clean,
  west_ford_clean,
  fish_cove_clean
  ) %>% 
  select(Date, well_depth_m, well_id, max_depth_m, flag)
  

test <- output %>%
  select(Date, well_depth_m, well_id) %>%
  pivot_wider(
    id_cols      = Date,
    names_from   = well_id,
    values_from  = well_depth_m
  ) %>% 
  rename_with(~ str_replace_all(., " ", "_")) %>% 
  arrange(Date)
  
  
site_cols <- colnames(test)
site_cols <- site_cols[site_cols != 'Date']
make_site_ts(test, site_cols)

#4.0 Write the output -------------------------------------------------------

output <- output %>% 
  rename(date = Date,
         water_level = well_depth_m)

# Remapping well_ids for consistency with Audrey's metadata
id_map <- c(
  "Ross Pond"      = "Ross",
  "Brantley North" = "Brantley North",
  "Devils Den"     = "Devils Den",
  "Surprise Pond"  = "Surprise",
  "West Ford"      = "West Ford",
  "Fishcove"       = "Fish Cove"
)

# remap well_id
output <- output %>%
  mutate(well_id = recode(well_id, !!!id_map))

output_path <- 'D:/doe_pt_processing/data_ordway/clean_osbs_core_wells_pre2025.csv'
write_csv(output, output_path)

