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

checks <- make_checks_df(data_full, qaqc)
plot_checks(checks, site)

all_offset_names <- grep("offset_m_", all_cols, value=TRUE)
all_offset_dates <- grep("P_G/L_date_", all_cols, value=TRUE)
all_offset_cols <- c(all_offset_dates, all_offset_names)
all_offsets <- pivot_history %>% 
  select(all_of(all_offset_cols))

quick_plot_offset2(all_offsets)

## ------- C Revise water depth -----------------------------------------

offset_names_to_use <- all_offset_names[all_offset_names != "offset_m_1"]
offset_dates_to_use <- all_offset_dates[all_offset_dates != "P_G/L_date_1"]
offset_cols_to_use <- c(offset_names_to_use, offset_dates_to_use)
offsets_to_use <- pivot_history %>% 
  select(all_of(offset_cols_to_use))
quick_plot_offset2(offsets_to_use)

offsets_to_use <- offsets_to_use %>% 
  select(all_of(offset_names_to_use))

new_offset <- offsets_to_use %>%  unlist() %>% mean(na.rm = TRUE)

data_full <- data_full %>% 
  mutate(offset_vF = new_offset,
         depth_vF = sensor_depth - offset_vF)

## ------- D Plot the data with a revised offset -----------------------

make_site_ts(site_ts=data_full,
             y_vars=c("original_depth", "depth_vF"),
             qaqc)

data_out <- data_full %>% 
  select(c('Site_ID', 'Date', 'sensor_depth', 'original_depth', 'depth_avg', 'depth_vF', 'offset_vF'))

plot_checks(make_checks_df(data_out, qaqc), site)


data_out <- data_out %>% 
  select(-c('depth_avg', 'offset_vF')) %>% 
  rename(revised_depth = depth_vF) %>% 
  mutate(offset_version = "offset_vF") %>% 
  mutate(flag = 0,
         notes = "Dropped the first offset, used offset v2, v3, v4")

output_data <- bind_rows(output_data, data_out)  

rm(site, data, qaqc, pivot_history, status, data_full, offsets_to_use) 
rm(all_cols, all_offset_cols, all_offset_dates, all_offset_names, depth_cols)
rm(new_offset, not_to_plot, site)
rm(offset_cols_to_use, offset_dates_to_use, offset_names_to_use)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II) Site: 13_267 -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- "13_267"
data <- site_ts_from_xlsx_sheet(compiled_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_history <- fetch_pivot_history(meta_data_path, site)
status <- fetch_post_process_status(status_path, site)
print(status$Notes)

data_full <- calc_stages_from_offsets(data, pivot_history)

ts_cols = colnames(data_full)
print(ts_cols)

make_site_ts(site_ts=data_full, 
             y_vars = c("original_depth"), 
             qaqc_df = qaqc)

# Make a QAQC df
checks <- make_checks_df(data_full, qaqc)

rm(site, data, qaqc, pivot_history, status, data_full) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Fetch Site Specific Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Calculate water levels based on various offsets -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Plot a site -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Apply the correct offset version
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scratch Workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example I)
# One of the offset values is dramatically wrong
# Time series shits dramatically in November 2023

# Example II)
# Offset appears to have changed gradually over the deployment
#site <- unique_wetland_wells[2]

# Example III)
# Example where first iteration of offset is better
#site <- unique_wetland_wells[15]

# Example IV) 
# Show cases how/why the choice of offset is so impactfull
# November 2023 download causes big jump in the original data, becuase 
# the offset changed, but did it really??
#site <- unique_wetland_wells[12]

# Example V)
# site <- unique_wetland_wells[47]

# site <- "15_409" # One of AJ's sites with high error!

