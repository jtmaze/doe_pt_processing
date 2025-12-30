# This script's purpose is to compile and view the OSBS stage data sent by Josh E.

# There's some major changes which I implement:

# A) I recalculate the "meters of head" to "water depth at well" using well dimensions. 
# I also calculate the "indexed well depth" relative to the 2022 well locations, since many wells were moved. 
#  There's several reasons for this recalculation of well depths. First, some wells were damaged (e.g. by wildlife), 
# and new well dimensions were not used in recent calculations. Second, almost all wells were moved deeper 
# at somepoint. Josh used the wetland's deepest point (from DEM) to index the timeseries, for moved wells;
# however this not ideal. The major flaw with the DEM approach is that the datum is sensitive 
# to the chosen DEM, its error, and pre-processing (e.g., vegitation filtering). Furthermore, DOE 
# samples' location (soil cores, sensors, etc.) were measured in the field relative to the well location. 
# Having timeseries indexed to these well locations makes historical data more interpretable. 

# B) Institute a "flag" column, which denotes low confidence well measurements (flag = 1),
# post-2022 bottomed out days (flag = 2), pre-2022 bottomed out days (flag = 3), and
# equipment malfunctions (flag=5),

# 1.0 Libraries and file paths --------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
source('./scripts/qaqc_functions.R')

tz = "UTC"
raw_dir <- "D:/doe_pt_processing/data_ordway/previously_processed/uncleaned_osbs_all_wells_2018_2025.xlsx"

well_dimensions <- list()
well_h_vals <- list()
flags_conditions <- list()

output_timeseries <- list()

read_data_rename_cols <- function(data_dir, sheet_name) {
  # read the data
  temp <- read_xlsx(
    data_dir,
    sheet=sheet_name
  )
  
  temp <- temp %>% 
    select(Date_Time, `Meter of Head`) %>% 
    rename(
      timestamp = Date_Time,
      head_m = `Meter of Head`)
  
  return(temp)
}

apply_flags <- function(df, rules) {
  df$flag  <- 0L
  df$notes <- NA_character_
  
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, ]
    
    idx <- rep(TRUE, nrow(df))
    
    if (!is.na(rule$begin))
      idx <- idx & df$timestamp >= rule$begin
    if (!is.na(rule$end))
      idx <- idx & df$timestamp <= rule$end
    if (!is.na(rule$condition))
      idx <- idx & eval(parse(text = rule$condition), df)
    
    df$flag[idx]  <- rule$flag
    df$notes[idx] <- rule$note
  }
  
  df
}

# 2.0 Recalculate "depth_at_well", "indexed_well_depth" apply flags--------------------------------------------

#...2.1 Ross Pond -------------------------------------------------------

#...2.1.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Ross") %>% 
  mutate(well_id = "Ross Pond")

ross_well_dimensions <- tibble(
  well_id = "Ross Pond",
  version = c(1, 2, 3),
  offset_value = c(0.1, -0.6, -0.4), 
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06", "2022-03-09"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", "2022-03-09", NA), tz=tz)
)

ross_well_h <- tibble(
  well_id = "Ross Pond", 
  version = c(1, 2),
  h_value = c(0.5, 0.46),
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.1.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp >= ross_well_dimensions$begin_date[1] &
        timestamp <  ross_well_dimensions$end_date[1] ~ ross_well_dimensions$offset_value[1],
      
      timestamp >= ross_well_dimensions$begin_date[2] &
        timestamp <  ross_well_dimensions$end_date[2] ~ ross_well_dimensions$offset_value[2],
      
      timestamp >= ross_well_dimensions$begin_date[3] ~
        ross_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.1.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= ross_well_h$begin_date[2] ~ well_depth_m + (ross_well_h$h_value[1] - ross_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )
  
#...2.1.D) Apply flags to the data -------------------------------------------------------

ross_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2022-03-09", NA, "Prior to well moving; treat data with extreme caution",
  5, "2019-05-23", "2019-07-26", NA, "equipment malfunction",
  5, "2021-06-16", "2022-03-09", NA, "equipment malfunction",
  3, NA, "2020-10-06", "well_depth_m < 0.14", "well bottomed out at old position (prior Oct, 2020)",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(ross_flag_rules)

#...2.1.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'well_depth_m')

# Remove a few problematic dates
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['Ross Pond']] <- df
flags_conditions[["Ross Pond"]] <- ross_flag_rules
well_dimensions[['Ross Pond']] <- ross_well_dimensions
well_h_vals[["Ross Pond"]] <- ross_well_h

rm(df, ross_flag_rules, ross_well_dimensions, ross_well_h)

#...2.2 Devils Den -------------------------------------------------------

#...2.2.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Devils Den") %>% 
  mutate(well_id = "Devils Den")

devils_den_well_dimensions <- tibble(
  well_id = "Devils Den",
  version = c(1, 2),
  offset_value = c(0.05, -0.90), 
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

devils_den_well_h <- tibble(
  well_id = "Devils Den", 
  version = c(1, 2),
  h_value = c(0.73, 0.29),
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.2.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < devils_den_well_dimensions$end_date[1] ~ devils_den_well_dimensions$offset_value[1],
      timestamp >= devils_den_well_dimensions$begin_date[2] ~ devils_den_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.2.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= devils_den_well_h$begin_date[2] ~ well_depth_m + (devils_den_well_h$h_value[1] - devils_den_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.2.D) Apply flags to the data -------------------------------------------------------

devils_den_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2020-10-06", NA, "Prior to well moving; treat data with extreme caution",
  3, NA, "2020-10-06", "well_depth_m < 0.15", "well bottomed out at old position (prior Oct, 2020)",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(devils_den_flag_rules)

#...2.2.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'well_depth_m')

# Remove a problematic date
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['Devils Den']] <- df
flags_conditions[["Devils Den"]] <- devils_den_flag_rules
well_dimensions[['Devils Den']] <- devils_den_well_dimensions
well_h_vals[["Devils Den"]] <- devils_den_well_h

rm(df, devils_den_flag_rules, devils_den_well_dimensions, devils_den_well_h)

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


#...2.3.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Brantley North") %>% 
  mutate(well_id = "Brantley North")

brantley_north_well_dimensions <- tibble(
  well_id = "Brantley North",
  version = c(1, 2, 3),
  offset_value = c(-0.01, -0.86, -0.80), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-07", "2022-03-03"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07","2022-03-03", NA), tz=tz)
)

brantley_north_well_h <- tibble(
  well_id = "Brantley North", 
  version = c(1, 2),
  h_value = c(0.43, 0.17),
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-07"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", NA), tz=tz)
)

#...2.3.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < brantley_north_well_dimensions$end_date[1] ~ brantley_north_well_dimensions$offset_value[1],
      
      timestamp >= brantley_north_well_dimensions$begin_date[2] &
        timestamp <= brantley_north_well_dimensions$end_date[2] ~ brantley_north_well_dimensions$offset_value[2],
      
      timestamp >= brantley_north_well_dimensions$begin_date[3] ~ brantley_north_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.3.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= brantley_north_well_h$begin_date[2] ~ well_depth_m + (brantley_north_well_h$h_value[1] - brantley_north_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.3.D) Apply flags to the data -------------------------------------------------------

brantley_north_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2020-10-07", NA, "Prior to well moving; treat data with extreme caution",
  1, "2021-06-01", "2022-03-03", NA, "Tape holding logger snapped, stage artificially high at some points. Could not find exact timepoint.",
  3, NA, "2020-10-07", "well_depth_m < 0.05", "well bottomed out at old position (prior Oct, 2020)",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(brantley_north_flag_rules)

#...2.3.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), "indexed_well_depth_m")

# Clean some of the anomalously bad observations
df <- df %>% 
  filter(
    (timestamp <  as.POSIXct("2020-01-09 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-01-10 00:00:00", tz = "UTC")) &
    (timestamp <  as.POSIXct("2020-10-07 00:00:00", tz = "UTC") |
       timestamp >= as.POSIXct("2020-10-08 00:00:00", tz = "UTC"))
  )

output_timeseries[["Brantley North"]] <- df
well_dimensions[["Brantley North"]] <- brantley_north_well_dimensions
well_h_vals[["Brantley North"]] <- brantley_north_well_h
flags_conditions[["Brantley North"]] <- brantley_north_flag_rules

rm(df, brantley_north_well_h, brantley_north_well_dimensions, brantley_north_flag_rules)

#...2.4 Surprise Pond -------------------------------------------------------

#...2.4.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Surprise") %>% 
  mutate(well_id = "Surprise")

surprise_well_dimensions <- tibble(
  well_id = "Surprise",
  version = c(1, 2, 3),
  offset_value = c(0, -0.63, -0.51), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-07", "2023-10-11"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", "2023-10-11", NA), tz=tz)
)

surprise_well_h <- tibble(
  well_id = "Surprise", 
  version = c(1, 2),
  h_value = c(1.57, 2.11),
  begin_date = as.POSIXct(c("2018-05-18", "2023-10-11"), tz=tz),
  end_date = as.POSIXct(c("2023-10-11", NA), tz=tz)
)

#...2.4.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < surprise_well_dimensions$end_date[1] ~ surprise_well_dimensions$offset_value[1],
      
      timestamp >= surprise_well_dimensions$end_date[1] &
        timestamp < surprise_well_dimensions$end_date[2] ~ surprise_well_dimensions$offset_value[2],
      
      timestamp >= surprise_well_dimensions$end_date[2] ~ surprise_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.4.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= surprise_well_h$begin_date[2] ~ well_depth_m + (surprise_well_h$h_value[1] - surprise_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )



#...2.4.D) Apply flags to the data -------------------------------------------------------

surprise_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2023-10-11", NA, "Prior to well moving; treat data with caution",
  5, "2019-06-03", "2019-08-07", NA, "Logger malfunction",
  5, "2021-06-06", "2022-03-03", NA, "Logger missing, shortage in lab"
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(surprise_flag_rules)

#...2.4.E) Check data and concatonate -------------------------------------------------------

# Clean some of the anomalously bad observations
df <- df %>% 
  filter(
    (timestamp <  as.POSIXct("2020-10-07 00:00:00", tz = "UTC") |
       timestamp >= as.POSIXct("2020-10-08 00:00:00", tz = "UTC")) &
      (timestamp <  as.POSIXct("2023-10-11 00:00:00", tz = "UTC") |
         timestamp >= as.POSIXct("2023-10-12 00:00:00", tz = "UTC"))
  )

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[["Surprise"]] <- df
well_dimensions[["Surprise"]] <- surprise_well_dimensions
well_h_vals[['Surprise']] <- surprise_well_h
flags_conditions[["Surprise"]] <- surprise_flag_rules

rm(df, surprise_well_dimensions, surprise_well_h, surprise_flag_rules)

#...2.5 West Ford -------------------------------------------------------

#...2.5.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "West Ford") %>% 
  mutate(well_id = "West Ford")

west_ford_well_dimensions <- tibble(
  well_id = "West Ford",
  version = c(1, 2, 3),
  offset_value = c(0.03, 0.05, -0.89), 
  begin_date = as.POSIXct(c("2018-05-16", "2020-01-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-01-16", "2020-10-06", NA), tz=tz)
)

west_ford_well_h <- tibble(
  well_id = "West Ford", 
  version = c(1, 2, 3),
  h_value = c(0.79, 0.48, 0.14),
  begin_date = as.POSIXct(c("2018-05-16", "2020-01-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-01-16", "2020-10-06", NA), tz=tz)
)

#...2.5.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < west_ford_well_dimensions$end_date[1] ~ west_ford_well_dimensions$offset_value[1],
      
      timestamp >= west_ford_well_dimensions$begin_date[2] &
        timestamp <= west_ford_well_dimensions$end_date[2] ~ west_ford_well_dimensions$offset_value[2],
      
      timestamp >= west_ford_well_dimensions$begin_date[3] ~ west_ford_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.5.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp < west_ford_well_h$begin_date[2] ~ well_depth_m + (west_ford_well_h$h_value[1] - west_ford_well_h$h_value[3]),
      
      timestamp >= west_ford_well_h$begin_date[2] &
        timestamp < west_ford_well_dimensions$begin_date[3] ~ well_depth_m + (west_ford_well_h$h_value[2] - west_ford_well_h$h_value[3]),
      
      TRUE ~ well_depth_m
    )
  )

#...2.5.D) Apply flags to the data -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'well_depth_m')


west_ford_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2020-10-06", NA, "Prior to well moving; treat data with extreme caution",
  3, NA, "2020-10-06", "well_depth_m < 0.09", "well bottomed out at old position (prior Oct, 2020)",
  5, "2019-05-23", "2019-07-26", NA, "Equipment malfunction."
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(west_ford_flag_rules)

#...2.5.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['West Ford']] <- df
well_dimensions[['West Ford']] <- west_ford_well_dimensions
well_h_vals[['West Ford']] <- west_ford_well_h
flags_conditions[['West Ford']] <- west_ford_flag_rules

rm(df, west_ford_well_dimensions, west_ford_well_h, west_ford_flag_rules)

#...2.6 Fish Cove -------------------------------------------------------

#...2.6.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Fish Cove") %>% 
  mutate(well_id = "Fish Cove")

fish_cove_well_dimensions <- tibble(
  well_id = "Fish Cove",
  version = c(1, 2, 3),
  offset_value = c(0.04, 0.07, -1.01), 
  begin_date = as.POSIXct(c("2018-05-18", "2019-06-03", "2022-03-02"), tz=tz),
  end_date = as.POSIXct(c("2019-06-03", "2022-03-02", NA), tz=tz)
)

fish_cove_well_h <- tibble(
  well_id = "Fish Cove", 
  version = c(1, 2, 3),
  h_value = c(0.91, 1.56, 1.02),
  begin_date = as.POSIXct(c("2018-05-18", "2019-06-03", "2022-03-02"), tz=tz),
  end_date = as.POSIXct(c("2019-06-03", "2022-03-02", NA), tz=tz)
)

#...2.6.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < fish_cove_well_dimensions$end_date[1] ~ fish_cove_well_dimensions$offset_value[1],
      
      timestamp >= fish_cove_well_dimensions$begin_date[2] &
        timestamp <= fish_cove_well_dimensions$end_date[2] ~ fish_cove_well_dimensions$offset_value[2],
      
      timestamp >= fish_cove_well_dimensions$begin_date[3] ~ fish_cove_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.6.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp < fish_cove_well_h$end_date[1] ~ well_depth_m + (fish_cove_well_h$h_value[1] - fish_cove_well_h$h_value[3]),
      
      timestamp > fish_cove_well_h$begin_date[2] &
        timestamp <= fish_cove_well_h$end_date[2] ~ well_depth_m + (fish_cove_well_h$h_value[2] - fish_cove_well_h$h_value[3]),
      
      timestamp > fish_cove_well_h$begin_date[3] ~ well_depth_m 
    )
  )

#...2.6.D) Apply flags to the data -------------------------------------------------------

fish_cove_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2022-03-03", NA, "Prior to well moving; treat data with extreme caution",
  5, "2019-06-02", "2019-07-26", NA, "Logger malfunction",
  5, "2020-01-16", "2022-03-03", NA, "Well not instrumented"
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(fish_cove_flag_rules)

#...2.6.E) Check data and concatonate -------------------------------------------------------

# Remove a problematic date
df <- df %>% 
  filter(
    timestamp < as.POSIXct("2019-06-03 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2019-06-04 00:00:00", tz = "UTC")
  )

make_site_ts(df %>% rename(date = timestamp) %>% filter(flag == 0), 'indexed_well_depth_m')

output_timeseries[['Fish Cove']] <- df
well_dimensions[['Fish Cove']] <- fish_cove_well_dimensions
well_h_vals[['Fish Cove']] <- fish_cove_well_h
flags_conditions[['Fish Cove']] <- fish_cove_flag_rules

rm(df, fish_cove_flag_rules, fish_cove_well_dimensions, fish_cove_well_h)

#...2.7 Anderson -------------------------------------------------------

#...2.7.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Anderson") %>% 
  mutate(well_id = "Anderson")

anderson_well_dimensions <- tibble(
  well_id = "Anderson",
  version = c(1, 2),
  offset_value = c(0.03, -0.59), 
  begin_date = as.POSIXct(c("2018-05-16", "2021-06-16"), tz=tz),
  end_date = as.POSIXct(c("2021-06-16", NA), tz=tz)
)

anderson_well_h <- tibble(
  well_id = "Anderson", 
  version = c(1, 2),
  h_value = c(1.37, 0.7),
  begin_date = as.POSIXct(c("2018-05-16", "2021-06-16"), tz=tz),
  end_date = as.POSIXct(c("2021-06-16", NA), tz=tz)
)

#...2.7.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < anderson_well_dimensions$end_date[1] ~ anderson_well_dimensions$offset_value[1],
      timestamp >= anderson_well_dimensions$begin_date[2] ~ anderson_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.7.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= anderson_well_h$begin_date[2] ~ well_depth_m + (anderson_well_h$h_value[1] - anderson_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.7.D) Apply flags to the data -------------------------------------------------------
make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

anderson_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2021-06-17", NA, "Older data (2018-2021) was very problematic. Deleted.",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(anderson_flag_rules) %>% 
  filter(timestamp > "2021-06-17")

#...2.7.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

df <- df %>% 
  filter(
    (timestamp <  as.POSIXct("2022-04-05 00:00:00", tz = "UTC") |
       timestamp >= as.POSIXct("2022-04-06 00:00:00", tz = "UTC")) &
      (timestamp <  as.POSIXct("2022-04-19 00:00:00", tz = "UTC") |
         timestamp >= as.POSIXct("2022-04-20 00:00:00", tz = "UTC")) &
        (timestamp <  as.POSIXct("2022-04-22 00:00:00", tz = "UTC") |
           timestamp >= as.POSIXct("2022-04-23 00:00:00", tz = "UTC")) &
          (timestamp <  as.POSIXct("2022-06-22 00:00:00", tz = "UTC") |
             timestamp >= as.POSIXct("2022-06-23 00:00:00", tz = "UTC")) &
            (timestamp <  as.POSIXct("2023-04-03 00:00:00", tz = "UTC") |
               timestamp >= as.POSIXct("2023-04-04 00:00:00", tz = "UTC")) &
              (timestamp <  as.POSIXct("2023-04-21 00:00:00", tz = "UTC") |
                 timestamp >= as.POSIXct("2023-04-22 00:00:00", tz = "UTC"))
  )

output_timeseries[['Anderson']] <- df
flags_conditions[['Anderson']] <- anderson_flag_rules
well_dimensions[['Anderson']] <- anderson_well_dimensions
well_h_vals[['Anderson']] <- anderson_well_h

rm(df, anderson_flag_rules, anderson_well_dimensions, anderson_well_h)

#...2.8 Fox -------------------------------------------------------

#...2.8.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Fox") %>% 
  mutate(well_id = "Fox")

fox_well_dimensions <- tibble(
  well_id = "Fox",
  version = c(1, 2),
  offset_value = c(0.03, -0.69), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

fox_well_h <- tibble(
  well_id = "Fox", 
  version = c(1, 2),
  h_value = c(0.6, 0.29),
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.8.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < fox_well_dimensions$end_date[1] ~ fox_well_dimensions$offset_value[1],
      timestamp >= fox_well_dimensions$begin_date[2] ~ fox_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.8.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= fox_well_h$begin_date[2] ~ well_depth_m + (fox_well_h$h_value[1] - fox_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.8.D) Apply flags to the data -------------------------------------------------------

fox_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2020-10-06", NA, "Prior to well moving; treat data with extreme caution",
  3, NA, "2020-10-06", "well_depth_m < 0.10", "well bottomed out at old position (prior Oct, 2020)",
  5, "2019-05-23", "2019-07-26", NA, "Logger malfunction",
  5, "2022-08-01", "2023-03-16", NA, "Logger malfunction"
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(fox_flag_rules)

#...2.8.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp) %>% filter(flag == 0), 'indexed_well_depth_m')

# Remove a problematic date
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['Fox']] <- df
flags_conditions[['Fox']] <- fox_flag_rules
well_dimensions[['Fox']] <- fox_well_dimensions
well_h_vals[['Fox']] <- fox_well_h

rm(df, fox_flag_rules, fox_well_dimensions, fox_well_h)

#...2.9 One shot -------------------------------------------------------

#...2.9.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "One Shot") %>% 
  mutate(well_id = "One Shot")

one_shot_well_dimensions <- tibble(
  well_id = "One Shot",
  version = c(1, 2),
  offset_value = c(0.02, -0.95), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

one_shot_well_h <- tibble(
  well_id = "One Shot", 
  version = c(1, 2),
  h_value = c(0.74, 0.75),
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.9.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < one_shot_well_dimensions$end_date[1] ~ one_shot_well_dimensions$offset_value[1],
      timestamp >= one_shot_well_dimensions$begin_date[2] ~ one_shot_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.9.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= one_shot_well_h$begin_date[2] ~ well_depth_m + (one_shot_well_h$h_value[1] - one_shot_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.9.D) Apply flags to the data -------------------------------------------------------

one_shot_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2021-06-16", NA, "Prior to well moving; treat data with extreme caution",
  5, "2019-06-03", "2019-08-07", NA, "Logger malfunction",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(one_shot_flag_rules)

#...2.9.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a problematic date
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['One Shot']] <- df
flags_conditions[['One Shot']] <- one_shot_flag_rules
well_dimensions[['One Shot']] <- one_shot_well_dimensions
well_h_vals[['One Shot']] <- one_shot_well_h

rm(df, one_shot_flag_rules, one_shot_well_dimensions, one_shot_well_h)

#...2.10 Harry Prairie -------------------------------------------------------

#...2.10.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Harry Prairie") %>% 
  mutate(well_id = "Harry Prairie")

harry_prairie_well_dimensions <- tibble(
  well_id = "Harry Prairie",
  version = c(1, 3, 2),
  offset_value = c(0.03, 0.06, -0.82), 
  begin_date = as.POSIXct(c("2018-05-16", "2020-07-17", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-07-17", "2020-10-06", NA), tz=tz)
)

harry_prairie_well_h <- tibble(
  well_id = "Harry Priarie", 
  version = c(1, 2),
  h_value = c(0.81, 0.30),
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.10.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < harry_prairie_well_dimensions$end_date[1] ~ harry_prairie_well_dimensions$offset_value[1],
      
      timestamp >= harry_prairie_well_dimensions$begin_date[2] &
        timestamp < harry_prairie_well_dimensions$end_date[2] ~ harry_prairie_well_dimensions$offset_value[2],
      
      timestamp >= harry_prairie_well_dimensions$begin_date[3] ~ harry_prairie_well_dimensions$offset_value[3]
                                                          
    ),
    well_depth_m = head_m + offset_value
  )

#...2.10.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= harry_prairie_well_h$begin_date[2] ~ well_depth_m + (harry_prairie_well_h$h_value[1] - harry_prairie_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.10.D) Apply flags to the data -------------------------------------------------------

# NOTE: there are no flags on this site, becuase I was able to continously trace indexed well movements. 
harry_prairie_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(harry_prairie_flag_rules)

#...2.10.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a problematic date
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['Harry Prairie']] <- df
flags_conditions[['Harry Prairie']] <- harry_prairie_flag_rules
well_dimensions[['Harry Prairie']] <- harry_prairie_well_dimensions
well_h_vals[['Harry Prarie']] <- harry_prairie_well_h

rm(df, harry_prairie_flag_rules, harry_prairie_well_dimensions, harry_prairie_well_h)

#...2.11 Small -------------------------------------------------------

#...2.11.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Small") %>% 
  mutate(well_id = "Small")

small_well_dimensions <- tibble(
  well_id = "Small",
  version = c(1, 2, 3),
  offset_value = c(0.02, -0.54, -1.81), 
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06", "2023-03-16"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06","2023-03-16", NA), tz=tz)
)

small_well_h <- tibble(
  well_id = "Small", 
  version = c(1, 2),
  h_value = c(0.21, 0.35),
  begin_date = as.POSIXct(c("2018-05-16", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.11.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < small_well_dimensions$end_date[1] ~ small_well_dimensions$offset_value[1],
      
      timestamp >= small_well_dimensions$begin_date[2] &
        timestamp <= small_well_dimensions$end_date[2] ~ small_well_dimensions$offset_value[2],
      
      timestamp >= small_well_dimensions$begin_date[3] ~ small_well_dimensions$offset_value[3]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.11.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= small_well_h$begin_date[2] ~ well_depth_m + (small_well_h$h_value[1] - small_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.11.D) Apply flags to the data -------------------------------------------------------

small_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2023-03-16", NA, "Prior to well moving; treat data with extreme caution",
  5, "2019-05-23", "2019-07-26", NA, "Logger malfunction",
  5, "2019-10-09", "2021-06-16", NA, "Logger not instrumented"
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(small_flag_rules)

#...2.11.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a few problematic dates
df <- df %>% 
  filter(
    (timestamp <  as.POSIXct("2022-11-10 00:00:00", tz = "UTC") |
       timestamp >= as.POSIXct("2022-11-11 00:00:00", tz = "UTC")) &
      (timestamp <  as.POSIXct("2023-02-05 00:00:00", tz = "UTC") |
         timestamp >= as.POSIXct("2023-02-06 00:00:00", tz = "UTC")) & 
        (timestamp <  as.POSIXct("2023-03-16 00:00:00", tz = "UTC") |
           timestamp >= as.POSIXct("2023-03-17 00:00:00", tz = "UTC")) 
  )

output_timeseries[['Small']] <- df
flags_conditions[['Small']] <- small_flag_rules
well_dimensions[['Small']] <- small_well_dimensions
well_h_vals[['Small']] <- small_well_h

rm(df, small_flag_rules, small_well_dimensions, small_well_h)

#...2.12 Hansford -------------------------------------------------------

#...2.12.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Hansford") %>% 
  mutate(well_id = "Hansford")

hansford_well_dimensions <- tibble(
  well_id = "Hansford",
  version = c(1),
  offset_value = c(-0.7), 
  begin_date = as.POSIXct(c("2020-10-07"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

hansford_well_h <- tibble(
  well_id = "Hansford", 
  version = c(1),
  h_value = c(0.41),
  begin_date = as.POSIXct(c("2020-10-07"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.12.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp > hansford_well_dimensions$begin_date[1] ~ hansford_well_dimensions$offset_value[1],
    ),
    well_depth_m = head_m + offset_value
  )

#...2.12.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(indexed_well_depth_m = well_depth_m) #NOTE: well did not move

#...2.12.D) Apply flags to the data ------------------------------------------------------- 

#NOTE: There is not an indexed (flag = 1), becuase well never changed locations. 
hansford_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  5, "2021-06-21", "2022-03-02", NA, "Well not instrumented", 
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(hansford_flag_rules)

#...2.12.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Hansford']] <- df
flags_conditions[['Hansford']] <- hansford_flag_rules
well_dimensions[['Hansford']] <- hansford_well_dimensions
well_h_vals[['Hansford']] <- hansford_well_h

rm(df, hansford_flag_rules, hansford_well_dimensions, hansford_well_h)

#...2.13 Breezeway -------------------------------------------------------

#...2.13.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Breezeway") %>% 
  mutate(well_id = "Breezeway")

breezeway_well_dimensions <- tibble(
  well_id = "Breezeway",
  version = c(1, 2),
  offset_value = c(0.01, -0.67), 
  begin_date = as.POSIXct(c("2018-03-28", "2020-10-07"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", NA), tz=tz)
)

breezeway_well_h <- tibble(
  well_id = "Breezeway", 
  version = c(1),
  h_value = c(0.2),
  begin_date = as.POSIXct(c("2018-03-28"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.13.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < breezeway_well_dimensions$end_date[1] ~ breezeway_well_dimensions$offset_value[1],
      timestamp >= breezeway_well_dimensions$begin_date[2] ~ breezeway_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.13.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(indexed_well_depth_m = well_depth_m) #NOTE: well did not move locations, just dug deeper only PG & PL adjusted

#...2.13.D) Apply flags to the data ------------------------------------------------------- 

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
breezeway_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  3, NA, "2020-10-07", "well_depth_m < 0.08", "well bottomed out at higher hanging (prior Oct, 2020)",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(breezeway_flag_rules)

#...2.13.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-07 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-08 00:00:00", tz = "UTC")
  )

output_timeseries[["Breezeway"]] <- df
flags_conditions[["Breezeway"]] <- breezeway_flag_rules
well_dimensions[['Breezeway']] <- breezeway_well_dimensions
well_h_vals[['Breezeway']] <- breezeway_well_h

rm(df, breezeway_flag_rules, breezeway_well_dimensions, breezeway_well_h)

#...2.14 Huey -------------------------------------------------------

#...2.14.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Huey") %>% 
  mutate(well_id = "Huey")

huey_well_dimensions <- tibble(
  well_id = "Huey",
  version = c(1, 2),
  offset_value = c(0.05, -0.64), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-07"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", NA), tz=tz)
)

huey_well_h <- tibble(
  well_id = "Huey", 
  version = c(1),
  h_value = c(0.77),
  begin_date = as.POSIXct(c("2018-05-18"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.14.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < huey_well_dimensions$end_date[1] ~ huey_well_dimensions$offset_value[1],
      timestamp >= huey_well_dimensions$begin_date[2] ~ huey_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.14.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(indexed_well_depth_m = well_depth_m) #NOTE: well did not move locations, just dug deeper only PG & PL adjusted

#...2.14.D) Apply flags to the data ------------------------------------------------------- 

# Remove a really bad stretch where the logger malfunctioned
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2021-01-16 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2022-03-09 00:00:00", tz = "UTC")
  )

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
huey_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  5, "2021-01-15", "2022-03-09", NA, "Logger malfunction",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(huey_flag_rules)

#...2.14.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a bad a download day where the logger was out of the well. 
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-07 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-08 00:00:00", tz = "UTC")
  )

output_timeseries[['Huey']] <- df
flags_conditions[['Huey']] <- huey_flag_rules
well_dimensions[['Huey']] <- huey_well_dimensions
well_h_vals[['Huey']] <- huey_well_h

rm(df, huey_flag_rules, huey_well_dimensions, huey_well_h)

#...2.15 Shady -------------------------------------------------------

#...2.15.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Shady") %>% 
  mutate(well_id = "Shady")

shady_well_dimensions <- tibble(
  well_id = "Shady",
  version = c(1, 2),
  offset_value = c(0.04, -0.64), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-07"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", NA), tz=tz)
)

shady_well_h <- tibble(
  well_id = "Shady", 
  version = c(1),
  h_value = c(1.28),
  begin_date = as.POSIXct(c("2018-05-18"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.15.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < shady_well_dimensions$end_date[1] ~ shady_well_dimensions$offset_value[1],
      timestamp >= shady_well_dimensions$begin_date[2] ~ shady_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.15.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(indexed_well_depth_m = well_depth_m) #NOTE: well did not move locations, just dug deeper only PG & PL adjusted

#...2.15.D) Apply flags to the data ------------------------------------------------------- 

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
shady_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  5, "2019-06-03", "2019-08-07", NA, "Logger malfunction",
  5, "2021-06-16", "2022-03-09", NA, "Not instrumented",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(shady_flag_rules)

#...2.15.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a bad a download day where the logger was out of the well. 
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-07 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-08 00:00:00", tz = "UTC")
  )

output_timeseries[['Shady']] <- df
flags_conditions[['Shady']] <- shady_flag_rules
well_dimensions[['Shady']] <- shady_well_dimensions
well_h_vals[['Shady']] <- shady_well_h

rm(df, shady_flag_rules, shady_well_dimensions, shady_well_h)

#...2.16 Blue -------------------------------------------------------

#...2.16.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Blue") %>% 
  mutate(well_id = "Blue")

blue_well_dimensions <- tibble(
  well_id = "Blue",
  version = c(1, 2),
  offset_value = c(0.02, -0.69), 
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-07", NA), tz=tz)
)

blue_well_h <- tibble(
  well_id = "Blue", 
  version = c(1, 2),
  h_value = c(1.24, 0.44),
  begin_date = as.POSIXct(c("2018-05-18", "2020-10-06"), tz=tz),
  end_date = as.POSIXct(c("2020-10-06", NA), tz=tz)
)

#...2.16.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = case_when(
      timestamp < blue_well_dimensions$end_date[1] ~ blue_well_dimensions$offset_value[1],
      timestamp >= blue_well_dimensions$begin_date[2] ~ blue_well_dimensions$offset_value[2]
    ),
    well_depth_m = head_m + offset_value
  )

#...2.16.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = case_when(
      timestamp <= blue_well_h$begin_date[2] ~ well_depth_m + (blue_well_h$h_value[1] - blue_well_h$h_value[2]),
      TRUE ~ well_depth_m
    )
  )

#...2.16.D) Apply flags to the data ------------------------------------------------------- 

blue_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note,
  1, NA, "2025-10-06", NA, "Whole timeseries is shakey, especially after Oct, 2020.",
  5, "2019-06-03", "2019-08-07", NA, "Logger malfunction",
  3, NA, "2020-10-06", "well_depth_m < 0.05", "well bottomed out at old position (prior Oct, 2020)",
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(blue_flag_rules)

#...2.16.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

# Remove a single erroneous day from well re-digging, and PT above ground.
df <- df %>% 
  filter(
    timestamp <  as.POSIXct("2020-10-06 00:00:00", tz = "UTC") |
      timestamp >= as.POSIXct("2020-10-07 00:00:00", tz = "UTC")
  )

output_timeseries[['Blue']] <- df
flags_conditions[['Blue']] <- blue_flag_rules
well_dimensions[['Blue']] <- blue_well_dimensions
well_h_vals[['Blue']] <- blue_well_h

rm(df, blue_flag_rules, blue_well_dimensions, blue_well_h)

#...2.17 Brantley East -------------------------------------------------------

#...2.17.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Brantley East") %>% 
  mutate(well_id = "Brantley East")

brantley_east_well_dimensions <- tibble(
  well_id = "Brantley East",
  version = c(1),
  offset_value = c(-0.53), 
  begin_date = as.POSIXct(c("2023-10-31"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

brantley_east_well_h <- tibble(
  well_id = "Brantley East", 
  version = c(1),
  h_value = c(NA),
  begin_date = as.POSIXct(c("2023-10-31"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.17.B) Read data compile well metadata -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = brantley_east_well_dimensions$offset_value[1],
    well_depth_m = head_m + offset_value
  )

#...2.17.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = well_depth_m
 )

#...2.17.D) Apply flags to the data -------------------------------------------------------

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
brantley_east_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note, # Not flagged data points to input
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(brantley_east_flag_rules)

#...2.17.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Brantley East']] <- df
flags_conditions[['Brantley East']] <- brantley_east_flag_rules
well_dimensions[['Brantley East']] <- brantley_east_well_dimensions
well_h_vals[['Brantley East']] <- brantley_east_well_h

rm(df, brantley_east_flag_rules, brantley_east_well_dimensions, brantley_east_well_h)

#...2.18 Breezeway Sandhill -------------------------------------------------------

#...2.18.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Breezeway Sandhill") %>% 
  mutate(well_id = "Breezeway Sandhill")

breezeway_sandhill_well_dimensions <- tibble(
  well_id = "Breezeway Sandhill",
  version = c(1),
  offset_value = c(-0.54), 
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

breezeway_sandhill_well_h <- tibble(
  well_id = "Breezeway Sandhill", 
  version = c(1),
  h_value = c(NA),
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.18.B) Read data compile well metadata -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = breezeway_sandhill_well_dimensions$offset_value[1],
    well_depth_m = head_m + offset_value
  )

#...2.18.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = well_depth_m
  )

#...2.18.D) Apply flags to the data -------------------------------------------------------

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
breezeway_sandhill_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note, # No flagged data points to input
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(breezeway_sandhill_flag_rules)

#...2.18.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Breezeway Sandhill']] <- df
flags_conditions[['Breezeway Sandhill']] <- breezeway_sandhill_flag_rules
well_dimensions[['Breezeway Sandhill']] <- breezeway_sandhill_well_dimensions
well_h_vals[['Breezeway Sandhill']] <- breezeway_sandhill_well_h

rm(df, breezeway_sandhill_flag_rules, breezeway_sandhill_well_dimensions, breezeway_sandhill_well_h)

#...2.19 Clear -------------------------------------------------------

#...2.19.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Clear") %>% 
  mutate(well_id = "Clear")

clear_well_dimensions <- tibble(
  well_id = "Clear",
  version = c(1),
  offset_value = c(-0.48), 
  begin_date = as.POSIXct(c("2023-10-31"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

clear_well_h <- tibble(
  well_id = "Clear", 
  version = c(1),
  h_value = c(NA),
  begin_date = as.POSIXct(c("2023-10-31"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.19.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = clear_well_dimensions$offset_value[1],
    well_depth_m = head_m + offset_value
  )

#...2.19.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = well_depth_m
  )

#...2.19.D) Apply flags to the data -------------------------------------------------------

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
clear_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note, # No flagged data points to input
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(clear_flag_rules)

#...2.19.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Clear']] <- df
flags_conditions[['Clear']] <- clear_flag_rules
well_dimensions[['Clear']] <- clear_well_dimensions
well_h_vals[['Clear']] <- clear_well_h

rm(df, clear_flag_rules, clear_well_dimensions, clear_well_h)

#...2.20 Enslow -------------------------------------------------------

#...2.20.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Enslow") %>% 
  mutate(well_id = "Enslow")

enslow_well_dimensions <- tibble(
  well_id = "Enslow",
  version = c(1),
  offset_value = c(-0.50), 
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

enslow_well_h <- tibble(
  well_id = "Enslow", 
  version = c(1),
  h_value = c(NA),
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.20.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = enslow_well_dimensions$offset_value[1],
    well_depth_m = head_m + offset_value
  )

#...2.20.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = well_depth_m
  )

#...2.20.D) Apply flags to the data -------------------------------------------------------

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
enslow_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note, # No flagged data points to input
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(enslow_flag_rules)

#...2.20.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Enslow']] <- df
flags_conditions[['Enslow']] <- enslow_flag_rules
well_dimensions[['Enslow']] <- enslow_well_dimensions
well_h_vals[['Enslow']] <- enslow_well_h

rm(df, enslow_flag_rules, enslow_well_dimensions, enslow_well_h)

#...2.21 Gopher -------------------------------------------------------

#...2.21.A) Read data compile well metadata -------------------------------------------------------

df <- read_data_rename_cols(raw_dir, "Gopher") %>% 
  mutate(well_id = "Gopher")

gopher_well_dimensions <- tibble(
  well_id = "Gopher",
  version = c(1),
  offset_value = c(-0.76), 
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

gopher_well_h <- tibble(
  well_id = "Gopher", 
  version = c(1),
  h_value = c(NA),
  begin_date = as.POSIXct(c("2023-10-27"), tz=tz),
  end_date = as.POSIXct(c(NA), tz=tz)
)

#...2.21.B) Calculate depth at well -------------------------------------------------------

df <- df %>% 
  mutate(
    offset_value = gopher_well_dimensions$offset_value[1],
    well_depth_m = head_m + offset_value
  )

#...2.21.C) Calculate the indexed well depth -------------------------------------------------------

df <- df %>% 
  mutate(
    indexed_well_depth_m = well_depth_m
  )

#...2.21.D) Apply flags to the data -------------------------------------------------------

# NOTE well did not change locations, so we can be more confident in measurements (no flag=1 data). 
gopher_flag_rules <- tribble(
  ~flag, ~begin, ~end, ~condition, ~note, # No flagged data points to input
) %>% 
  mutate(
    begin = as.POSIXct(begin, tz = tz),
    end   = as.POSIXct(end, tz = tz)
  )

df <- df %>% 
  apply_flags(gopher_flag_rules)

#...2.21.E) Check data and concatonate -------------------------------------------------------

make_site_ts(df %>% rename(date = timestamp), 'indexed_well_depth_m')

output_timeseries[['Gopher']] <- df
flags_conditions[['Gopher']] <- gopher_flag_rules
well_dimensions[['Gopher']] <- gopher_well_dimensions
well_h_vals[['Gopher']] <- gopher_well_h

rm(df, gopher_flag_rules, gopher_well_dimensions, gopher_well_h)

# 3.0 Write all of the ouput --------------------------------------------

out_dir <- "D:/doe_pt_processing/data_ordway/temp/"

updated_ts <- bind_rows(output_timeseries)
well_dimensions <- bind_rows(well_dimensions)
well_h_vals <- bind_rows(well_h_vals)
flag_info <- bind_rows(flags_conditions, .id="well_id")

ts_path <- paste0(out_dir, "osbs_compiled_reindexed_well_depths.csv")
well_dimensions_path <- paste0(out_dir, "osbs_dimensions.csv")
well_h_path <- paste0(out_dir, "osbs_well_h_vals.csv")
flag_info_path <- paste0(out_dir, "osbs_well_timeseries_flags.csv")

write_csv(updated_ts, ts_path)
write_csv(well_dimensions, well_dimensions_path)
write_csv(well_h_vals, well_h_path)
write_csv(flag_info, flag_info_path)


