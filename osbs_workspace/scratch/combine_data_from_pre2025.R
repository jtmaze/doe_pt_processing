# This script's purpose is to compile and view the OSBS stage data sent by Josh E.
# Essentially, I generate one long dataframe, rename columns, check the time series, and 
# concatenate everything into .csv instead of an excel workbook. 

# There's two note-worthy changes to the data

# 1) I make a new column called "well_depth_indexed" this column is designed to hind-cast
# pre-2020 well depth (before the wells were re-dug). I apply a consistent datum, so the well depth
# is all indexed to the latest well. 

# 2) I flag the older data whenever the well was moved and the time series could be
# inconsistent. When wells were initially installed (post-Irma) water levels were
# exceptionally high, many wells needed to be moved in subsequent years.

# 1.0 Libraries and file paths --------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
source('./scripts/qaqc_functions.R')

stage_dir <- "D:/doe_pt_processing/data_ordway/previously_processed/uncleaned_osbs_all_wells_2018_2025.xlsx"

offsets <- list()

# 2.0 Data read and cleaning ----------------------------------------------

select_rename_cols <- function(df, site_name) {
  temp <- df %>%
    select(
      Date_Time,
      `Meter of Head`,
      `Well Water depth (m)`,
      `Max Depth (m)`,
    ) %>%
    rename(
      date = Date_Time,
      head_m = `Meter of Head`,
      well_depth_m = `Well Water depth (m)`,
      basin_low_depth_m = `Max Depth (m)`,
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
  # the well measurements and datum. 
  
  ranges <- stage_df %>%
    arrange(date) %>% 
    filter(!is.na(offset_val)) %>% 
    group_by(offset_val) %>% 
    summarize(
      begin_date = min(date, na.rm = TRUE),
      end_date = max(date, na.rm = TRUE),
      .groups = 'drop'
    ) #%>% 
    # mutate(
    #   end_date = if_else(
    #     end_date == max(end_date),
    #     as.POSIXct(NA),
    #     end_date
    #   )
    # )
  
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

# .......2.1.A Inspect time series and well measurement/datum changes  -------------------------------------------------------

ross_pond <- read_xlsx(
  stage_dir,
  sheet = 'Ross'
)

ross_pond <- select_rename_cols(ross_pond, 'Ross Pond')
tz = attr(ross_pond$date, 'tzone')
make_site_ts(ross_pond, y_var='well_depth_m')

# Clean a handfull of erroneous data points
ross_pond <- ross_pond %>% 
  filter(
    date <= as.POSIXct('2020-10-6 12:00:00', tz = tz) | 
      date >= as.POSIXct('2020-10-6 15:00:00', tz = tz)
  )

# Inspect all the offsets (conversions from head to well depth)
offset_val <- unique(ross_pond$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Ross Pond', 
  stringsAsFactors = FALSE
)

offsets[['Ross Pond']] <- find_offset_begin_end(ross_pond, offsets_temp, 'Ross Pond')

# .......2.1.B Produce the indexed well depth with consistent datum  -------------------------------------------------------

# Difference between well and basin low for each well install
well_h1 <- 0.5 
well_h2 <- 0.46
well_h3 <- 0.46

ross_pond <- ross_pond %>% 
  mutate(
    well_depth_indexed = case_when(
      date <= as.Date("2020-10-06") ~ well_depth_m + (well_h1 - well_h3), 
      date > as.Date("2020-10-06") & date < as.Date("2022-03-09") ~ well_depth_m + (well_h2 - well_h3),
      date > as.Date("2022-03-09") ~ well_depth_m 
    )
  )
make_site_ts(ross_pond, c('well_depth_m', 'well_depth_indexed', 'basin_low_depth_m'))

# .......2.1.C Flag the timeseries  -------------------------------------------------------

ross_pond <- ross_pond %>% 
  mutate(
    flag = case_when(
      # equipment malfunction periods
      date >= as.Date("2019-05-23") & date <= as.Date("2019-07-26") ~ 5,
      date >= as.Date("2021-06-16") & date <= as.Date("2022-03-09") ~ 5,
      # well bottomed out at old position
      date < as.Date("2020-10-06") & well_depth_m < 0.14 ~ 3,
      # prior to well moving
      date < as.Date("2022-03-03") ~ 1,
      # Quality data
      TRUE ~ 0
    ),
    # Update depths  
    well_depth_m = case_when(
      flag == 5 ~ NA_real_,
      flag %in% c(1, 3) ~ well_depth_m,
      TRUE ~ well_depth_m
    ),
    well_depth_indexed = case_when(
      flag == 5 ~ NA_real_,
      flag %in% c(1, 3) ~ well_depth_indexed,
      TRUE ~ well_depth_indexed
    ),
    basin_low_depth_m = case_when(
      flag == 5 ~ NA_real_,
      flag %in% c(1, 3) ~ basin_low_depth_m,
      TRUE ~ basin_low_depth_m
    ),
    notes = case_when(
      flag == 5 ~ "equipment malfunction",
      flag == 3 ~ "well bottomed out at old position (prior Oct, 2020)",
      flag == 1 ~ "Prior to well moving; treat data with extreme caution. Elevation change of only 4cm indicates DEM error.",
      TRUE ~ NA_character_
    )
  )

# Select only the output columns
ross_pond_clean <- ross_pond %>% 
  select(c('date','head_m','well_id','well_depth_m','well_depth_indexed','basin_low_depth_m','flag','notes'))

make_site_ts(ross_pond_clean %>% filter(flag == 0), 'well_depth_m')

rm(ross_pond, offsets_temp, offset_val, versions, well_h1,
   well_h2, well_h3)

#...2.2 Devils Den -------------------------------------------------------

# DATA PROCESSING NOTES:
# - Level logger bottomed out from March 2020 to October 2020. 
# Well moved to a deeper location on 10/16/2020

# - Well was too deep to access to 3/28/25

# .......2.2.A Inspect time series and well measurement/datum changes  -------------------------------------------------------

devils_den <- read_xlsx(
  stage_dir,
  sheet = 'Devils Den'
)

devils_den <- select_rename_cols(devils_den, 'Devils Den')
make_site_ts(devils_den, y_var='basin_low_depth_m')
# Clean a handfull of erroneous data points with well moving
devils_den <- devils_den %>% 
  filter(
    date <= as.POSIXct('2020-10-5 00:00:00', tz = tz) | 
      date >= as.POSIXct('2020-10-6 23:00:00', tz = tz)
  )

offset_val <- unique(devils_den$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Devils Den', 
  stringsAsFactors = FALSE
)

offsets[["Devils Den"]] <- find_offset_begin_end(devils_den, offsets_temp, 'Devils Den')

# .......2.2.B Produce the indexed well depth with consistent datum  -------------------------------------------------------

well_h1 <- 0.73
well_h2 <- 0.29

devils_den <- devils_den %>% 
  mutate(
    well_depth_indexed = case_when(
      date <= as.Date("2020-10-06") ~ well_depth_m + (well_h1 - well_h2), 
      date > as.Date("2020-10-06") ~ well_depth_m 
    )
  )

make_site_ts(devils_den, c('well_depth_m', 'well_depth_indexed', 'basin_low_depth_m'))

# .......2.2.C Flag the timeseries  -------------------------------------------------------

devils_den <- devils_den %>% 
  mutate(
    flag = case_when(
      # well bottomed out at old position
      date < as.Date("2020-10-06") & well_depth_m < 0.15 ~ 3,
      # prior to well moving
      date < as.Date("2020-10-06") ~ 1,
      TRUE ~ 0
    ),
    notes = case_when(
      flag == 3 ~ "well bottomed out at old position (prior Oct, 2020)",
      flag == 1 ~ "prior to well moving; treat data with some caution",
      TRUE ~ NA_character_
    )
  )

devils_den_clean <- devils_den %>% 
  select(c('date','head_m', 'well_id', 'well_depth_m', 'well_depth_indexed', 'basin_low_depth_m', 'flag', 'notes'))

make_site_ts(devils_den_clean %>% filter(flag == 0), 'well_depth_m')

rm(devils_den, offsets_temp, offset_val, versions, well_h1, well_h2)
  
#...2.3 Brantley North -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Negative (anamolous) values observed on 1/7/20, 1/8/20 and 1/9/20. 
# They were removed from time series (level logger malfunction). 

# - 3/19/20 to 10/7/20 level logger bottomed out prompting deeper installation on 10/7/2020

# - 3/3/22: The tape adhering the logger to the well (to allow it to suspend) was ripped. 
# The logger was resting on the well bottom. Reattached tape and the new L to P is 128 cm. 
# This has not yet been applied to the time series (both the new L to P and
# finding the exact time when the logger string snapped). 
# ******* Follow-up too difficult to find snap-event (compared with other wells)
# ******* if snap happened durring rain event, we'll never know when. Flagged all data prior to Mar, 2022.

# .......2.3.A Inspect time series and well measurement/datum changes  -------------------------------------------------------
brantley_north <- read_xlsx(
  stage_dir,
  sheet = 'Brantley North'
)

brantley_north <- select_rename_cols(brantley_north, 'Brantley North')
make_site_ts(brantley_north, y_var='basin_low_depth_m')
# Filter erroneous time periods around well switch
brantley_north <- brantley_north %>% 
  filter(
    date <= as.POSIXct('2020-10-7 00:00:00', tz = tz) | 
      date >= as.POSIXct('2020-10-7 23:00:00', tz = tz)
  )

offset_val <- unique(brantley_north$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Brantley North', 
  stringsAsFactors = FALSE
)

offsets[["Brantley North"]]<- find_offset_begin_end(brantley_north, offsets_temp, 'Brantley North')

# NOTE adjusting later depth values based on site notes from Josh
offsets[["Brantley North"]][2, "end_date"] <- as.POSIXct("2022-03-03 00:00:00", tz="UTC")
p_g <- 0.48
p_l <- 1.28
offset_new <- p_l - p_g
offsets[["Brantley North"]] <- offsets[["Brantley North"]] %>% 
  bind_rows(
    tibble(
      offset_val = offset_new,
      versions = 3,
      well_id = "Brantley North",
      begin_date = as.POSIXct("2022-03-03 00:00:00", tz = "UTC"),
      end_date   = NA
    )
  )
# Recalculate with the appropriate offset.
# Old offset prior to tape breaking used in basin_low calculation.
offset2 <- offsets[["Brantley North"]][2, "offset_val"]
brantley_north <- brantley_north %>% 
  mutate(
    offset_val = if_else(
      date > as.Date("2020-10-07") & date <= as.Date("2022-03-03"),
      offset_new,
      offset_val
    ),
    well_depth_m = if_else(
      date > as.Date("2020-10-07") & date <= as.Date("2022-03-03"),
      head_m - offset_new,
      well_depth_m
    ),
    basin_low_depth_m = if_else(
      date > as.Date("2020-10-07") & date <= as.Date("2022-03-03"),
      basin_low_depth_m - (offset2 - offset_new),
      basin_low_depth_m
    )
  )

# .......2.3.B Produce the indexed well depth with consistent datum  -------------------------------------------------------

well_h1 <- 0.43
well_h2 <- 0.17

brantley_north <- brantley_north %>% 
  mutate(
    well_depth_indexed = case_when(
      date <= as.Date("2020-10-07") ~ well_depth_m + (well_h1 - well_h2), 
      date > as.Date("2020-10-07") ~ well_depth_m 
    )
  )

make_site_ts(brantley_north, c('well_depth_m', 'well_depth_indexed', 'basin_low_depth_m'))

# .......2.3.C Flag the timeseries  -------------------------------------------------------

brantley_north <- brantley_north %>%
  mutate(
    flag = case_when(
      # well bottomed out at old position
      date < as.Date("2020-10-07") & well_depth_m < 0.05 ~ 3,
      # prior to well moving AND tape snapping event
      date <= as.Date("2022-03-03") ~ 1,
      TRUE ~ 0
    ),
    notes = case_when(
      flag == 3 ~ "well bottomed out at old position (prior Oct, 2020)",
      flag == 1 ~ "prior to well moving and tape snap event (Mar 2022); treat data with some caution",
      TRUE ~ NA_character_
    )
  )

brantley_north_clean <- brantley_north %>% 
  select(c('date','head_m', 'well_id', 'well_depth_m', 'well_depth_indexed' ,'basin_low_depth_m', 'flag', 'notes'))

make_site_ts(brantley_north_clean %>% filter(!flag == 3), 'well_depth_indexed')

rm(brantley_north, offsets_temp, offset_new, offset2, offset_val, 
   p_g, p_l, versions, well_h1, well_h2)

#...2.4 Surprise -------------------------------------------------------

# DATA PROCESSING NOTES:

# - Logger malfunction due to HOBO shuttle issue between 6/3/19 to 8/7/19. Data not retrievable

# - Well was without a logger from 6/16/21 until 3/3/22 due to logger shortage in lab (Onset level logger backorder)

# - Well disturbed (crooked but still in place) but logger string was snapped 
# upon arrival during the Oct 2023 visit (possible gator/animal activity). 
# The well had to be pulled to retrieve the level logger at the bottom of the well. 
# The well was reinstalled in a new location closer to shore (current location)

# .......2.4.A Inspect time series and well measurement/datum changes  -------------------------------------------------------
surprise <- read_xlsx(
  stage_dir,
  sheet = 'Surprise'
)

surprise <- select_rename_cols(surprise, 'Surprise Pond')
make_site_ts(surprise, y_var='basin_low_depth_m')

# Clean a handfull of erroneous data points
surprise <- surprise %>% 
  filter(
    date <= as.POSIXct('2020-10-06 23:00:00', tz = tz) | 
      date >= as.POSIXct('2020-10-08 00:00:00', tz = tz)
  ) %>% 
  filter(
    date < as.POSIXct('2023-10-11 00:00:00', tz = tz) | 
      date >= as.POSIXct('2023-10-12 23:00:00', tz = tz)
  )


# Check the offset values. 
offset_val <- unique(surprise$offset_val)
offset_val <- Filter(Negate(is.na), offset_val)
versions <- seq(from=1, to=length(offset_val), by=1)

offsets_temp <- data.frame(
  offset_val,
  versions,
  well_id='Surprise', 
  stringsAsFactors = FALSE
)

offsets[["Surprise"]] <- find_offset_begin_end(surprise, offsets_temp, 'Surprise')

# Remove bad offset used after 2020 and adjust manually.
offsets[["Surprise"]] <- offsets[["Surprise"]] %>% 
  filter(!begin_date >= as.Date("2020-10-01"))
print(offsets["Surprise"])

date_1 <- as.POSIXct('2020-10-07 00:00:00', tz='UTC')
date_2 <- as.POSIXct('2023-10-11 10:00:00', tz='UTC')
offset_May2018_Oct2020 <- 0
offset_Oct2020_Oct2023 <- -0.63
offset_Oct2023_curr <- -0.59

well_h1 <- 1.57
well_h2 <- 2.11

surprise <- surprise %>% 
  mutate(
    # Step 1: Determine the correct offset scalar based on date
    offset_val = case_when(
      date < date_1 ~ offset_May2018_Oct2020,
      date < date_2 ~ offset_Oct2020_Oct2023, # Catches everything between date_1 and date_2
      TRUE          ~ offset_Oct2023_curr      # "TRUE" acts as the "Else" catch-all
    ),
    # Step 2: Calculate depth using the offset determined above
    well_depth_m = head_m + offset_val,
    # Step 3: Handle the basin logic
    well_depth_indexed = case_when(
      date < date_1 ~ well_depth_m + (offset_May2018_Oct2020 - ),
      date < date_2 ~ offset_Oct2020_Oct2023, # Catches everything between date_1 and date_2
      TRUE          ~ offset_Oct2023_curr      # "TRUE" acts as the "Else" catch-all
    ),
    basin_low_depth_m = case_when(
      date < date_2 ~ well_depth_m + well_h1,
      TRUE          ~ well_depth_m + well_h2
    )
  )

make_site_ts(surprise, 'basin_low_depth_m')

# .......2.4.B Produce the indexed well depth with consistent datum  -------------------------------------------------------


# .......2.4.C Flag the timeseries  -------------------------------------------------------

surprise <- surprise %>% 
  mutate(
    flag = case_when(
      # equipment malfunction periods
      date >= as.Date("2019-05-23") & date <= as.Date("2019-07-26") ~ 5,
      date >= as.Date("2021-06-16") & date <= as.Date("2022-03-09") ~ 5,
      date < as.Date("2020-10-07") ~ 1,
    ),
    # change well_depth and basin_min_depth to reflect equipment malfunction
    well_depth_m = case_when(
      flag == 5 ~ NA_real_,
      TRUE ~ well_depth_m
    ),
    basin_low_depth_m = case_when(
      flag == 5 ~ NA_real_,
      TRUE ~ well_depth_m
    ),
    notes = case_when(
      flag == 5 ~ "Equipment malfunction, PT died.",
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
  ) %>% 
  select(date, well_depth_m, well_id, basin_low_depth_m, flag)
  

test <- output %>%
  select(date, well_depth_m, well_id) %>%
  pivot_wider(
    id_cols      = date,
    names_from   = well_id,
    values_from  = well_depth_m
  ) %>% 
  rename_with(~ str_replace_all(., " ", "_")) %>% 
  arrange(date)
  
  
site_cols <- colnames(test)
site_cols <- site_cols[site_cols != 'date']
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

