
# 1.0 Libraries and file paths --------------------------------------------
library(tidverse)
raw_data_dir <- "D:/doe_pt_processing/data_ordway/raw_files/"
baro_dir <- "D:/doe_pt_processing/data_ordway/baro_files/"
output_path <- "D:/doe_pt_processing/data_ordway/fall2025_downloads_to_check_offset.csv"
raw_files <- list.files(path=raw_data_dir, pattern='LL.csv', full.names=TRUE)
baro_files <- list.files(path=baro_dir, pattern='.csv', full.names=TRUE)

# 2.0 Concatenate the raw files and baro files --------------------------------------------

combined_raw_files <- data.frame()

for(f in raw_files){
  df <- read_csv(f, col_types = cols(`#` = col_skip()), skip=1)
  df <- df[ ,c(1, 2)]
  colnames(df)[1] <- "Date"
  colnames(df)[2] <- "PT"
  df$Date <- mdy_hms(df$Date)
  well_id <- strsplit(basename(f), '_LL')[[1]][1]
  well_id <- gsub("_", " ", well_id)
  df$well_id <- well_id
  
  combined_raw_files <- rbind(combined_raw_files, df)
}

combined_baro_files <- data.frame()

for(f in baro_files){
  df <- read_csv(f) %>% 
    rename(Date = Period,
           baro_millibar = `BP avg (mb)`) %>% 
    mutate(Date = dmy_hm(Date, tz = 'UTC')) %>% 
    select(c(Date, baro_millibar))
  
  combined_baro_files <- rbind(combined_baro_files, df)
}

# 3.0 Merge baro data with PT data --------------------------------------------

# Convert baro pressure units (millibar) to match the PTs (PSI)
combined_baro_files <- combined_baro_files %>% 
  mutate(baro_psi = baro_millibar * 0.0145038) %>% 
  select(-c(baro_millibar))

combined_raw_files <- left_join(combined_raw_files, combined_baro_files)

# 4.0 Merge baro data with PT data --------------------------------------------

# Convert the gauge pressure to absolute pressure and water depth
combined_raw_files <- combined_raw_files %>% 
  mutate(p_gauge = PT - baro_psi,
         sensor_depth = 1000 * p_gauge / 2.2 /(2.54^2) / 100) %>% 
  select(-c(p_gauge, baro_psi, PT))

# 5.0 Write the depth dataframe to check offsets in next file -------------------

write_csv(combined_raw_files, output_path)
  

