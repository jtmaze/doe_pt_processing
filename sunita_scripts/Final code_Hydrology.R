
#### Code by Esther Lee and Sunita Shrestha


#####################calculation of spill threshold, recession rate and PTI and PTC 


# navigate to where the excel file (wetland water level) is located
setwd()
getwd()
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Wetlands/Wetland_H20_level/Data_processed/Basin 3")


#########################################################################

# Load data from a CSV file in the same folder (You will need to specify the file location and name)
library(openxlsx)
library(readxl)
df <- read_excel("3_311_WD.xlsx")
View(df)

#########################################################################

# plot timeseries data of wetland water depth (cm)
plot(df$`Date Time`,df$`water depth (cm)`,type = "l")
plot(df$`Date Time`,df$`water depth (cm)`,type = "l",ylim = c(-60,-40))


# Replace values less than -0.06 with NA
df$`water depth (cm)`[df$`water depth (cm)` < -42] <- NA

# Print updated data frame
plot(df$`Date Time`,df$`water depth (cm)`,type = "l")
View(df)
write.xlsx(df, "3_173_WD (cm)_exceeding_max_deleted.xlsx")



# Load data from a CSV file in the same folder (You will need to specify the file location and name)

library(readxl)
df1 <- read_excel("15_409_WD (cm).xlsx")
View(df1)

#########################################################################

# plot timeseries data of wetland water depth (cm)
plot(df1$`Date Time`,df1$`wetland water depth (cm)`,type = "l")
plot(df1$`Date Time`,df1$`wetland water depth (cm)`,type = "l",ylim = c(-48,-40))


# Replace values less than -0.06 with NA
df1$`wetland water depth (cm)`[df1$`wetland water depth (cm)` < -30] <- NA

# Print updated data frame
plot(df1$`Date Time`,df1$`wetland water depth (cm)`,type = "l")
View(df1)
write.xlsx(df1, "15_409_WD (cm)_exceeding_max_deleted.xlsx")



###################### Extract night time data ######################
# extracting data from 8 am to 11 pm #####
nighttime_data <- subset(df, format(`Date Time`, "%H:%M") >= "21:28:46" | format(`Date Time`, "%H:%M") <= "08:28:46")

# Print the extracted nighttime data
# View (nighttime_data)


# plot timeseries data of wetland water depth (cm)
# plot(nighttime_data$`Date Time`,nighttime_data$`Water level (cm)`,type = "l")
# EL: why there is no gap for the daytime??
# SS : I didn't get the question, it is only nighttime data i have extracted. 

# change column name to enable calling a variable (Q. Why we did this?)
colnames(nighttime_data) <- c("datetime", "waterlevel_cm")


############# 
# Initialize daily_time as an empty vector
daily_time <- numeric(0)

# Reshape the original vector into a matrix with 11 rows and ?? columns ( = length of 'waterlevel_cm' / 11)
daily_time <- matrix(nighttime_data$waterlevel_cm, ncol = length(nighttime_data$waterlevel_cm)/11)  #EL: update the value of there aren't 11 points for each nighttime

#EL: check if this is integer
ncol = length(nighttime_data$waterlevel_cm)/11
ncol

# Convert the matrix to a data frame if needed
daily_df <- as.data.frame(daily_time)

num_cols <- ncol(daily_df)
############# EL end

#############
# 1. start from 'daily_df' and fit a linear regression line for each column ( = 1 block of nighttime (11pm-8am))
# 2. save the column if the slope of the regression line is negative (using 'if statement')
############# EL note ends


#### 1/10/2024 #########
#### Fitting Linear Regression to each column 

# Create an empty list to store regression models
regression_models <- list()

# Loop through each column and fit a linear regression
for (i in 1:num_cols) {
  # Extract the column
  column_data <- daily_df[[i]]
  
  View(column_data)
  # Create a time index
  time_index <- 1:length(column_data) # 1 through 11 (which means each time index is 1 hour)
  
  # Fit linear regression
  regression_model <- lm(column_data ~ time_index)
  
  # Save the regression model in the list
  regression_models[[paste0("Column_", i)]] <- regression_model   # EL: paste0 function is used to concatenate strings  together without any separator
}

# Now 'regression_models' is a list containing linear regression models for each column

head(regression_models)

############ save columns with negative slopes

# Create an empty list to store columns with negative slopes
negative_slope_columns <- list()

# Loop through each regression model
for (i in 1:num_cols) {
  # Extract each regression model one at a time
  regression_model <- regression_models[[paste0("Column_", i)]]
  
  # Extract the slope coefficient from the regression model
  slope <- coef(regression_model)[2]  # Assuming the time index is the second coefficient
  
  # insert a code to save the slopes for each column 'recessionrate_columns'
  
  # Check if the slope is negative
  if (slope < 0) {
    # If negative, save the corresponding column from daily_df ( = water level in cm)
    negative_slope_columns[[paste0("Column_", i)]] <- daily_df[[i]]  # values in negative_slope_columns are water level (cm), not slope
  }
}

# Now 'negative_slope_columns' is a list containing columns with negative slopes
View(negative_slope_columns)




# ####### Plot recession rate i.e negative slope vs stage
# # mean nighttime wetland stage (cm) as X and nighttime recession rate (cm/h) as Y i.e negative slope columns.

# Create an empty list to store recession rates for each column
negative_slope_recession_rate <- list()
slope_column <- list()
negative_slope_waterlevel <- list()

# Loop through each regression model
for (i in 1:num_cols) {
  # Extract each regression model one at a time
  regression_model <- regression_models[[paste0("Column_", i)]]
  
  # Extract the slope coefficient from the regression model
  slope_column[i] <- coef(regression_model)[2]  # Assuming the time index is the second coefficient
  
  # Check if the slope is negative
  if (slope_column[i] < 0) {
    # If negative, save the corresponding column from daily_df ( = water level in cm)
    negative_slope_waterlevel[[paste0("Column_", i)]] <- daily_df[[i]]   #negative_slope_columns -> negative_slope_waterlevel
    negative_slope_recession_rate[[paste0("Column_", i)]] <- slope_column[[i]]
  }
}

negative_slope_waterlevel_df <- data.frame(negative_slope_waterlevel)  #( = water level in cm)
View(negative_slope_waterlevel_df)
negative_slope_recession_rate_df <- data.frame(negative_slope_recession_rate)  #( = water level in cm)
View(negative_slope_recession_rate_df)

#check if length of neg slope water level  =  length of neg slope recession rate
length(negative_slope_waterlevel_df)
ncol_neg = length(negative_slope_recession_rate_df)


#calculate daily mean of 'negative_slope_recession_rate_df'
negative_slope_waterlevel_mean <- list()

for (i in 1:ncol_neg){
  selected_column = negative_slope_waterlevel_df[,i]
  negative_slope_waterlevel_mean[i] <- mean(selected_column)
}

negative_slope_waterlevel_mean_df <- data.frame(negative_slope_waterlevel_mean)
View(negative_slope_waterlevel_mean_df)

# length(negative_slope_waterlevel_mean_df)  #checking the length of the mean water level


# Now 'recession_rate_vs_stage' contains mean stage and recession rate for each column with negative slopes

# final variables: 
# 1. negative_slope_waterlevel_mean_df
# 2. negative_slope_recession_rate_df


#check if nighttime recession rate is cm/h or cm/d (refer to Lee et al., 2023 WRR)
#### From Lee et al., 2023, I found that it is in cm /hr

#figure out why there is an error in plotting recession rate vs. water level

########## Creating a plot between recession rate and water level
# Extract numeric vectors from lists
x <- unlist(negative_slope_waterlevel_mean_df)
y <- unlist(negative_slope_recession_rate_df)

# Create a scatter plot
plot(x, y, xlab = "Mean Water level (cm)", ylab = "Recession rate (cm/h)")



# Sort data in ascending order (from low WL to high WL)

# Create a data frame
# x: negative_slope_waterlevel_mean_df
# y: negative_slope_recession_rate_df

df_loess <- data.frame(x, y)

# Sort the data frame based on the 'df_loess$x' column in ascending order
sorted_df_loess <- df_loess[order(df_loess$x), ]


# Fit a loess curve
loess_model <- loess(sorted_df_loess$y ~ sorted_df_loess$x)


# Predict values using the loess model
predicted_values <- predict(loess_model, newdata = data.frame(x,y))

# Plot the original data points and the loess curve
plot(sorted_df_loess$x, sorted_df_loess$y, 
     xlab = "Mean Nighttime Wetland Stage (cm)", ylab = "Nighttime Recession Rate (cm/h)",
     main = "5_546")    #EL: you need to double check the unit for the recession rate, wether it is cm/h or cm/d --> compare recesison rate values from Lee et al., 2023

lines(sorted_df_loess$x, predicted_values, col = "red", lwd = 2)


####### Identify break-point using segmented regression

# Install and load the segmented package 
if (!require(segmented)) {
  install.packages("segmented")
  library(segmented)
}

# Fit segmented regression model
x <- sorted_df_loess$x
y <- sorted_df_loess$y

segmented_model <- segmented(lm(y ~ x), 
                             seg.Z = ~ x, psi = c(30))  # 'psi' is initial guess of the break point
# summary(segmented_model)



# Plot the original data points, loess curve, and segmented regression
plot(x, y, 
     xlab = "Mean Nighttime Wetland Stage (cm)", ylab = "Nighttime Recession Rate (cm/d)",
     main = "5_546")

# Add the loess curve
lines(x, predicted_values, col = "red", lwd = 2)

# Add the segmented regression curve
# lines(x, segmented_model$fit, col = "blue", lwd = 2)

# Extract the breakpoint or critical value from the segmented model
spill_depth_cm <- segmented_model$psi[, 2]

# Print or use the critical value as needed
print(paste("Breakpoint (spill depth, cm)):", spill_depth_cm))


# Add a vertical dotted line at x = spill_depth_cm
abline(v = spill_depth_cm, col = "red", lty = 2)



####################################################################################
####### PTC and PTI of 2_23 wetland
df <- read_excel("2_23.xlsx")  
View(df)


# Create a logical index based on water level greater than 0
df$logical_index_inundation <- as.integer(df$`water depth (cm)` > 0)

# Display the result
View(df)

# sum of logical index inundation 
inundation_no <- sum(df$logical_index_inundation)
inundation_no

# Find the number of data points in the first column (month)
number_of_data_points <- length(df$datetime)
print(number_of_data_points)

# percent time inundation of wetland 2_23
(inundation_no/number_of_data_points)*100


####### PTC of wetland 2_23
# Create a logical index based on water level conditions
df$logical_index_connected <- ifelse(df$`water depth (cm)` > 0 & df$`water depth (cm)` > 58.371, 1, 0)

# Display the updated data frame
View(df)

# number of connected points
connected_no <- sum(df$logical_index_connected)
connected_no

# percent time connected
(connected_no/number_of_data_points)*100

