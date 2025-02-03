
############ Code by Sam Howley and Sunita Shrestha (8/20/2024) to extract water level time series

# load libraries------------------------------------------------------
library(anytime)
library(tidyverse)
library(readxl)

### set working directory------------------------------------------------
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project")


#compile all PT####---------------------------------------------------------
# locate files
file.names <- list.files(path="Wetlands/Wetland_H20_level/For R", pattern=".csv", full.names=TRUE)

# create a empty vector to store data---------------------------------------
PT_all<-data.frame()

# Process multiple files and data into a single data frame------------------------
for(fil in file.names){   # loops over fil in file.names
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)    # in File called fil, skip column with name # and also skip that column
  PT<-PT[,c(1,2)]    # select first and second columns from the data frame PT
  colnames(PT)[1] <- "Date"    # rename first column to date 
  colnames(PT)[2] <- "PT"      # rename 2nd column as PT
  PT$Date <- mdy_hms(PT$Date)  # convert date to date time
  PT$BasinID<-strsplit(basename(fil), '_')[[1]][1]   # extract BasinID from the file name fil and split into two separated by underscores and take the first part of the split results and assigned to a new column 'BasinID' 
  PT$WetlandID<-strsplit(basename(fil), '_')[[1]][2]  # extracts the WetlandID from the filename, which is the second part of the filename split by underscores, and assigns it to a new column WetlandID.
  PT_all<-rbind(PT_all,PT)}    # This appends the current data frame PT to the existing data frame PT_all. rbind is used to combine rows, so PT_all accumulates data from all processed files.



# change the value of BasinID in your PT_all data frame from '14/9' to '14.9'----------------------
PT_all$BasinID[PT_all$BasinID=='14/9']<-'14.9'#change name so its more compatible in R



#
PT_all <-  PT_all %>%
  mutate(region= case_when(BasinID=="6"|BasinID=="6a"|BasinID=="3"|
                          BasinID=="7"~ 'N',
                           BasinID=="5"|BasinID=="5a"|BasinID=="15"|
                          BasinID=="9"|BasinID=="14"|BasinID=="13"|
                          BasinID=="14.9"~ 'S'))

# Make a column called region using BasinID column 
#ID basins by their region for easy joining with baro
# If BasinID is one of the following values: "6", "6a", "3", or "7", the region column set to 'N'.
# If BasinID is one of these values: "5", "5a", "15", "9", "14", "13", or "14.9", the region column set to 'S'.


write_csv(PT_all, "Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")


#### Now I have compiled PT (absolute pressure of pressure transducer by date)



########################
#Form dataframe (df)####
########################
PT<-read_csv("Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")
PT <- PT %>%
  mutate(Site = paste(BasinID, WetlandID, sep = "_")) %>%
  arrange(Site, Date) %>% mutate(day=as.Date(Date))

baro<-read_csv('Wetlands/Wetland_H20_level/Data_processed/compiled_baro.csv')



master<-left_join(PT, baro, by=c('region','Date'))
master <- master[!duplicated(master[c('Date','Site')]),]


# check<-PT%>%filter(Site=='13_271')
# ggplot(check, aes(Date, PT)) +
#   geom_line() 





Wetland_well_metadata <- read_excel("Masterfiles_latest/Wetland_well_metadata.xlsx",sheet = "Wetland_pivot_history")

#Format metadata to be compatable with master df
chk1<-Wetland_well_metadata %>% select(Site_ID,Basin_ID,`P_G/L_date_1`,
                                       P_G_cm_1,P_L_cm_1) %>% rename('PG'="P_G_cm_1", 'PL'='P_L_cm_1', 'Date'='P_G/L_date_1')

chk2<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_2`,
                                      P_G_cm_2,P_L_cm_2)%>% rename('PG'="P_G_cm_2", 'PL'='P_L_cm_2', 'Date'='P_G/L_date_2')

chk3<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_3`,
                                      P_G_cm_3,P_L_cm_3)%>% rename('PG'="P_G_cm_3", 'PL'='P_L_cm_3', 'Date'='P_G/L_date_3')

chk4<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_4`,
                                      P_G_cm_4,P_L_cm_4)%>% rename('PG'="P_G_cm_4", 'PL'='P_L_cm_4', 'Date'='P_G/L_date_4')
PG_PL<-rbind(chk1,chk2,chk3,chk4)


PG_PL <- PG_PL %>%
  rename('day'='Date',"Site"="Site_ID")%>%select(day, PG, PL,Site)

master<-left_join(master, PG_PL, by=c('Site','day'))








#Arrange df to be in chronological order by site. Then fill down PG and PL values
master <- master %>% arrange(Site, Date)%>% fill(PG, PL)



#######################
#Calculate Stage#####
######################

master<-master %>% 
  mutate(Water_press=PT-PTbaro)%>% 
  mutate(sensor_depth=1000*Water_press/2.2/(2.54^2)/100, PG=as.numeric(PG), PL=as.numeric(PL))%>%
  mutate(depth=sensor_depth-(PL-PG)/100)%>%
  select(Date, Site, BasinID, Water_press, sensor_depth,depth, PT, PTbaro, PG,PL) %>%
  filter(depth> -5) %>% filter(depth<4)

ggplot(master, aes(x=Date, color=Site)) + geom_line(aes(y=depth))+facet_wrap(~ BasinID, ncol=5)


check<-master%>%filter(Site=='3_23')
ggplot(check, aes(Date, depth)) +
  geom_line() 



#Export excel
wl_list <- split(master, master$Site)
library(writexl)
library(openxlsx)
write.xlsx(wl_list, "Wetlands/Wetland_H20_level/Data_processed/compiled_stage.xlsx")


