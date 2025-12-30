#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("tidyverse")
library("plotly")
source("./scripts/qaqc_functions.R")

previous_path <- "D:/doe_pt_processing/data_ordway/clean_osbs_core_wells_pre2025.csv"
latest_path <- "D:/doe_pt_processing/data_ordway/temp/fall2025_downloads_to_check_offset.csv"
output_path <- "D:/doe_pt_processing/data_ordway/output/fall2025_processed_well_depth.csv"

master_df <- read_csv(previous_path) %>% 
  rename(Date = date) %>% 
  select(-c('max_depth_m'))

latest_data <- read_csv(latest_path)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I) Site: Ross -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 0.4
temp <- latest_data %>% 
  filter(well_id == 'Ross') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'Ross'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II) Site: Devils Den -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 0.90

temp <- latest_data %>% 
  filter(well_id == 'Devils Den') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'Devils Den'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III) Site: Brantley North -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 0.80

temp <- latest_data %>% 
  filter(well_id == 'Brantley North') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'Brantley North'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV) Site: Surprise -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 0.61

temp <- latest_data %>% 
  filter(well_id == 'Surprise') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'Surprise'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V) Site: West Ford -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 0.89

temp <- latest_data %>% 
  filter(well_id == 'West Ford') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'West Ford'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VI) Site: Fish Cove -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offset <- 1.01

temp <- latest_data %>% 
  filter(well_id == 'Fish Cove') %>% 
  mutate(water_level = sensor_depth - offset,
         flag = 0) %>% 
  select(-c('sensor_depth'))

master_df <- bind_rows(master_df, temp)

make_site_ts(master_df %>% filter(well_id == 'Fish Cove'), y_vars='water_level')

rm(temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VI) Plot all timeseries together -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_daily <- master_df %>%             
  mutate(Date = as.Date(Date)) %>%      # daily resolution
  filter(Date >= ymd('2022-03-03')) %>% 
  group_by(Date, well_id) %>%
  summarize(water_level = mean(water_level, na.rm = TRUE), .groups = "drop")


p_facets <- ggplot(df_daily, aes(x = Date, y = water_level)) +
  geom_line() +
  facet_wrap(~ well_id, scales = "free_y", ncol = 2) +
  labs(title = "Water level at well", x = "Date", y = "Well Depth (m)") +
  theme_minimal()

print(p_facets)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write the output file -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write_csv(master_df, output_path)



