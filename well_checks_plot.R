
### 
# Make a quick plot showing field check conformity
# before and after offset revisions
###
library(tidyverse)

df <- read_csv('./data/out_data/well_checks_log.csv')

# ----- Boxplots showing mean agreement

mean_df1 <- df %>% 
  filter(version != "depth_avg") %>% 
  group_by(version, Site_ID) %>% 
  summarise(mean_diff = mean(diff, na.rm=TRUE))

ggplot(mean_df1, aes(x = version, y = mean_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "blue") +
  labs(
    title = "Field Measurement Agreement by Dataset Version (all wells)",
    x = "Dataset Version",
    y = "Average Sensor - Field Measurement (all dates) in meters"
  ) + 
  theme_bw() +  
  scale_x_discrete(labels = c(
    "original_depth" = "Original Dataset", 
    "revised_depth"  = "Revised Dataset"
  )) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

ggplot(mean_df1, aes(x = version, y = mean_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "blue") +
  labs(
    title = "Field Measurement Agreement by Dataset Version (trimmed y-axis)",
    x = "Dataset Version",
    y = "Average Sensor - Field Measurement (all dates) in meters"
  ) + 
  theme_bw() +  
  scale_x_discrete(labels = c(
    "original_depth" = "Original Dataset", 
    "revised_depth"  = "Revised Dataset"
  )) +
  scale_y_continuous(limits = c(-0.20, 0.20)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

mean_df2 <- df %>% 
  group_by(version, Site_ID) %>% 
  summarise(mean_diff = mean(diff, na.rm=TRUE))

ggplot(mean_df2, aes(x = version, y = mean_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "blue") +
  labs(
    title = "Field Measurement Agreement by Dataset Version (trimmed y-axis)",
    x = "Dataset Version",
    y = "Average Sensor - Field Measurement (all dates) in meters"
  ) + 
  theme_bw() +  
  scale_x_discrete(labels = c(
    "depth_avg" = "Mean all well offset dates",
    "original_depth" = "Original Dataset", 
    "revised_depth"  = "Revised Dataset"
  )) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# --------------- Agreement with unflaggd data ------------------

timeseries <- read_csv('./data/out_data/waterlevel_offsets_tracked.csv')

unflagged_sites <- timeseries %>% 
  group_by(Site_ID) %>% 
  summarise(all_zero = all(flag == 0)) %>% 
  filter(all_zero) %>% 
  pull(Site_ID)

df_mean_unflagged <- df %>% 
  filter(Site_ID %in% unflagged_sites) %>% 
  filter(version == "revised_depth") %>% 
  group_by(Site_ID) %>% 
  summarize(mean_diff = mean(diff, na.rm = TRUE)) %>% 
  arrange(mean_diff)

ggplot(df_mean_unflagged, aes(x = reorder(Site_ID, mean_diff), y = mean_diff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Revised Data (Flag = 0) Mean Difference All Dates (Logger - Field) by Site_ID",
    x = "Site_ID",
    y = "Mean Difference (meters)"
  ) +
  theme_bw() +
  coord_flip()





