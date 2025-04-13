#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)

raw_dir <- './data/raw_files_F24-W25/'
raw_files <- list.files(path=raw_dir, pattern='LL.csv', full.names=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Compile all the csvs -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_raw_files <- data.frame()

for(f in raw_files){
  df <- read_csv(f, col_types = cols(`#` = col_skip()), skip = 1)
  df <- df[ ,c(1, 2)]
  colnames(df)[1] <- "Date"
  colnames(df)[2] <- "PT"
  df$Date <- mdy_hms(df$Date)
  df$BasinID <- strsplit(basename(f), '_')[[1]][1]
  df$WetlandID <- strsplit(basename(f), '_')[[1]][2]
  df$Site_ID <- paste(df$BasinID, df$WetlandID, sep = "_")
  combined_raw_files <- rbind(combined_raw_files, df)
  rm(df)
}

unique_wetlands <- unique(combined_raw_files$Site_ID)
unique_basins <- unique(combined_raw_files$BasinID)

print(unique_wetlands)
print(unique_basins)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Assign Baro Loggers to the Wetlands -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_raw_files$BasinID[combined_raw_files$BasinID == '14/9' | combined_raw_files$BasinID == 149] <- '14.9'

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
## 3.1 Compile all csvs for sites missing old data  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
missing_sites = c('13_274', '5_546', '7_622')
missing_df <- read_csv('./data/compiled_PT.csv') %>% 
  mutate(WetlandID = as.character(WetlandID),
         Site_ID = paste(BasinID, WetlandID, sep='_')) %>% 
  filter(Site_ID %in% missing_sites) %>% 
  rename(baro_region = region)

combined_raw_files <- bind_rows(combined_raw_files, missing_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.2 Round the Site_IDs where PTs aren't recording on the hour -------------------------------------------------------
## NOTE this bug caused several sites to not be processed in the earlier script -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

round_dates_not_on_hour <- function(df) {
  
  df$Date <- as.POSIXct(df$Date)
  # Round the Date column to the nearest hour to match with Baros
  df <- df %>% 
    mutate(
      rounded_time = round_date(Date, unit="hour"), 
      rounding_delta = as.numeric(difftime(rounded_time, Date, units='mins'))
    )
  df$Date <- df$rounded_time
  
  # Check the rounding deltas on the PTs
  # Is too high of a rounding delta problematic?
  rounding_info <- df %>% 
    filter(!is.na(PT)) %>% 
    filter(rounding_delta != 0) %>% 
    mutate(rounding_delta = round(rounding_delta)) %>% 
    select(Site_ID, rounding_delta) %>% 
    distinct()
  
  print(rounding_info, n=50)
  # Remove temporary columns, return the df
  df <- df %>%  select(-rounded_time, -rounding_delta)
  return(df)
}

combined_raw_files <- round_dates_not_on_hour(combined_raw_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.3 Join baro data with Wetland PT data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

baro_data <- read_csv(paste0(raw_dir, 'compiled_baro.csv')) %>% 
  rename(baro_region = region) %>% 
  distinct() # For some reason there were lots of duplicate baro observations for a given date and region

combined_raw_files <- left_join(
  combined_raw_files, 
  baro_data, 
  by=c('baro_region', 'Date'),
  relationship = 'many-to-many'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Calculate Water Height Above Sensor -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calc_water_height <- function(df) {
  out_df <- df %>% 
    mutate(Water_press = PT - PTbaro,
           sensor_depth = 1000 * Water_press / 2.2 /(2.54^2) /100)
  
  return(out_df)
}

compiled_pt_data <- calc_water_height(combined_raw_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5.0 Collate PG, PL and offset measurements -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wetland_well_meta <- read_excel(
  './data/Wetland_well_metadata_1.xlsx', 
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
  select(Site_ID, day, PG, PL)

rm(chk1, chk2, chk3, chk4, chk5, wetland_well_meta)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6.0 Dynamically apply offset overtime (NOTE BAD PROCEDURE) -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# !!! For finalized data we don't change the well's PG/PL and offset measurements
# !!! Only keeping this step for continuity of data. 

# Join PG_PL to the
compiled_pt_data <- compiled_pt_data %>% 
  mutate(day = as.Date(Date)) 
compiled_pt_data <- left_join(compiled_pt_data, PG_PL, by=c('Site_ID', 'day'))

compiled_pt_data <- compiled_pt_data %>% 
  arrange(Site_ID, Date) %>% 
  fill(PG, PL) 

compiled_pt_data <- compiled_pt_data %>% 
  mutate(
    # NOTE: if PG and PL aren't filled, I replace them with zero.
    # This does not matter since we no longer apply dynamic PG/PL measurements that change with time.
    PG = coalesce(as.numeric(PG), 0),
    PL = coalesce(as.numeric(PL), 0)
    ) %>% 
  mutate(depth = sensor_depth - (PL - PG) / 100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7.0 Bind the latest downloads to the old data  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compiled_pt_data <- compiled_pt_data %>% 
  rename(Site = Site_ID) %>% 
  select(Date, Site, BasinID, Water_press, sensor_depth, depth, PT, PTbaro, PG, PL)

# Read the previous data
previous_data_path <- './data/compiled_stage_2.xlsx'
sheet_names <- excel_sheets(previous_data_path)
previous_dfs <- lapply(sheet_names, function(sheet) {
  read_excel(previous_data_path, sheet=sheet)
})
previous <- bind_rows(previous_dfs)
rm(previous_dfs)
# NOTE: I'm recalculating water depth for pre-existing data, 
# becuase there was a suspect baro measurement from January 7th 2022 until 
# April 9th 2022 for sites using the South baro logger.

previous <- previous %>% 
  select(c("Date", "Site", "BasinID", "PT", "PG", "PL")) %>% 
  rename(Site_ID = Site)

previous <- assign_baro(previous)

# previous <- previous %>% 
#   mutate(
#     baro_region = case_when(
#       Date >= as.Date('2022-01-27') & Date <= as.Date('2022-04-09') ~ "N",
#       TRUE ~ baro_region
#     )
#   )

previous <- left_join(
  previous, 
  baro_data, 
  by=c('baro_region', 'Date'),
  relationship = 'many-to-many'
)

previous <- calc_water_height(previous) 

previous <- previous %>% 
  mutate(
    depth = sensor_depth - (PL - PG) / 100
  ) %>% 
  select(-baro_region) %>% 
  rename(Site = Site_ID)


compiled_pt_data <- bind_rows(compiled_pt_data, previous) %>% 
  filter(depth < 3)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8.0 Write the processed data  -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sheet_list <- split(compiled_pt_data, compiled_pt_data$Site)
write.xlsx(sheet_list, "./data/compiled_stage_JM.xlsx")


