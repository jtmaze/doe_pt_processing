library('sf')
library('dplyr')
library('readxl')

meta_data_path <- "./data/Wetland_well_metadata_1.xlsx"
rtk_data_path <- "./data/rtk_data_digitized.xlsx"
out_dir <- "./data/out_data/"


##### Make read the lat/long points from excel
coord_data <- read_excel(meta_data_path, sheet="Wetland_and_well_info")
  
coord_data_v1 <- coord_data %>% 
  select(c("Site_ID", "Latitude1", "Longitude1")) %>% 
  rename(
    site_id = Site_ID,
    lat = Latitude1,
    long = Longitude1
  )
  
coord_data_v2 <- coord_data %>%
  select(c("Site_ID", "Latitude2", "Longitude2")) %>% 
  rename(
    site_id = Site_ID,
    lat = Latitude2,
    long = Longitude2
  )

coord_data_v3 <- coord_data %>%
  select(c("Site_ID", "Latitude3", "Longitude3")) %>% 
  rename(
    site_id = Site_ID,
    lat = Latitude3,
    long = Longitude3
  )

coord_data_avg <- coord_data %>% 
  select(c("Site_ID", "Lat_AVG", "Long_AVG")) %>% 
  rename(
    site_id = Site_ID,
    lat = Lat_AVG,
    long = Long_AVG
  )


coord_data_rtk <- read_excel(rtk_data_path) %>% 
  select(c('site_id', 'latitude', 'longitude')) %>% 
  rename(
    lat=latitude,
    long=longitude
  )


  
#### Function to write the coordinates to a shapefile

points_to_shapefile <- function(points_df, version, out_dir){
  
  # Filter out points with no lat/lon pairs
  points_df <- points_df %>% 
    filter(!is.na(lat) & !is.na(long))
    
  # Convert the dataframe to simple feature object  
  points_sf <- st_as_sf(points_df,
                        coords= c("long", "lat"),
                        crs = 4326 #TODO: Is WGS84 correct????
  )
  
  # Write data to shapefile
  out_path = paste0(out_dir, "wetland_well_points_v", version, ".shp")
  st_write(points_sf,
           out_path,
           driver="ESRI Shapefile",
           append=FALSE)
}

#### Write each version as a shapefile

points_to_shapefile(coord_data_v1, "1", out_dir)
points_to_shapefile(coord_data_v2, "2", out_dir)
points_to_shapefile(coord_data_v3, "3", out_dir)
points_to_shapefile(coord_data_avg, "AVG", out_dir)
points_to_shapefile(coord_data_rtk, "RTK", out_dir)
