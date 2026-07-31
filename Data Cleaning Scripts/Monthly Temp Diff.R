## Script to look at temperature differences between adjacent timestamps across sites. Provides a mean temperature change
## and a standard deviation value for each site to be used for QC test thresholds.

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)

# Set current wd
setwd("C:/Users/zombi/Desktop/Capstone")

# Path to the CSV in current wd
path <- "YAQUINA_TEMP_QC_READY.csv"

# Read the CSV
df_regular <- read_csv(path, show_col_types = FALSE) %>%
  mutate(
    temp_raw = temp,
    month = month(datetime),
    month_name = month(datetime, label = TRUE, abbr = FALSE),
    month_day = format(datetime, "%m-%d"),
    year = year(datetime)
  )

# Calculate monthly mean water temperature by site for growing season May 01 to Nov 01
df_temp_mean <- df_regular %>%
    filter(
      month_day >= "05-01",
      month_day < "11-01",
      minute(datetime) %% 10 == 0,
      !is.na(temp_raw)
    ) %>%
  group_by(site_ID, month, month_name) %>%
  summarise(
    n_temp_values = n(),
    mean_temp_season = mean(temp_raw, na.rm = TRUE),
    sd_temp_season = sd(temp_raw, na.rm = TRUE),
    temp_upper_4sd = mean_temp_season + 4 * sd_temp_season,
    temp_lower_4sd = mean_temp_season - 4 * sd_temp_season,
    .groups = "drop"
  ) %>%
  arrange(site_ID, month)

write_csv(df_temp_mean, "Temp_Summary_Monthly.csv")

# Calculate subsequent temperature differences by site for growing season May 01 to Nov 01
df_temp_diff <- df_regular %>%
  filter(
    month_day >= "05-01",
    month_day < "11-01"
  ) %>%
  group_by(site_ID, year, month) %>% #applies test to values within the same site
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    temp_diff = temp_raw - lag(temp_raw), # lag is dplyr version, gives previous row
    time_diff_min = as.numeric(
      difftime(datetime, lag(datetime), units = "mins")
    ) 
  ) %>%
  ungroup()

# Summary stats by site

temp_diff_summary_monthly <- df_temp_diff %>%
  filter(time_diff_min == 10) %>% # Only use true adjacent 10-minute observations when estimating site variability 
  group_by(site_ID, month, month_name) %>%
  summarise(
    n_diff_values = sum(!is.na(temp_diff)),
    mean_temp_diff = mean(temp_diff, na.rm = TRUE),
    sd_temp_diff = sd(temp_diff, na.rm = TRUE),
    plus_2sd = mean_temp_diff + 2 * sd_temp_diff,
    minus_2sd = mean_temp_diff - 2 * sd_temp_diff,
    plus_4sd = mean_temp_diff + 4 * sd_temp_diff,
    minus_4sd = mean_temp_diff - 4 * sd_temp_diff,
    threshold_4sd_abs = abs(mean_temp_diff) + 4 * sd_temp_diff,
    .groups = "drop"
  )%>%
  arrange(site_ID, month)

print(temp_diff_summary_monthly)

write_csv(temp_diff_summary_monthly, "TEMP_DIFF_SUMMARY_MONTHLY.csv")






##Diagnostics
# Fraction of record assessment
# What fraction of records are true adjacent 10-minute observations
# df_temp_diff %>%
#   group_by(site_ID) %>%
#   summarise(
#     n_total = n(),
#     n_10min = sum(time_diff_min == 10, na.rm = TRUE),
#     pct_10min = 100 * n_10min / n_total
#   )
# 
# df_temp_diff %>%
#   filter(!is.na(temp_diff)) %>%
#   count(time_diff_min, sort = TRUE)

# Find all non-10-minute intervals
# non_10min_intervals <- df_temp_diff %>%
#   group_by(site_ID, year) %>%
#   arrange(datetime, .by_group = TRUE) %>%
#   mutate(
#     prev_datetime = lag(datetime),
#     prev_temp = lag(temp_raw)
#   ) %>%
#   ungroup() %>%
#   filter(
#     !is.na(time_diff_min),
#     time_diff_min != 10
#   ) %>%
#   select(
#     site_ID,
#     year,
#     prev_datetime,
#     datetime,
#     time_diff_min,
#     prev_temp,
#     temp_raw,
#     temp_diff
#   ) %>%
#   arrange(site_ID, datetime)

# Write to CSV
#write_csv(non_10min_intervals,"NON_10MIN_INTERVALS.csv")

###Plots

# Generate Q-Q Plots to check data against normal distribution
# ggplot(
#   df_temp_diff %>% filter(!is.na(temp_diff)),
#   aes(sample = temp_diff)
# ) +
#   stat_qq(size = 0.3) +
#   stat_qq_line(color = "red") +
#   facet_wrap(~ site_ID, scales = "free") +
#   theme_minimal()

# Histogram of differences by site

# site_to_plot <- "OF"
# 
# site_diff <- df_temp_diff %>%
#   filter(site_ID == site_to_plot)
# 
# site_diff_plot <- site_diff %>%
#   filter(is.finite(temp_diff))
# 
# site_summary <- temp_diff_summary %>%
#   filter(site_ID == site_to_plot)
# 
# # Pull summary stats
# mean_diff <- site_summary$mean_temp_diff
# sd_diff <- site_summary$sd_temp_diff
# 
# # Dynamic x-axis limits
# x_min <- mean_diff - 5 * sd_diff
# x_max <- mean_diff + 5 * sd_diff
# 
# # Dynamic bin width
# binwidth_value <- site_summary$sd_temp_diff/4
# 
# ggplot(site_diff, aes(x = temp_diff)) +
#   geom_histogram(
#     binwidth = binwidth_value, 
#     color = "white"
#   ) +
#   geom_vline(
#     data = site_summary,
#     aes(xintercept = mean_temp_diff),
#     linetype = "solid",
#     linewidth = 0.8
#   ) +
#   geom_vline(
#     data = site_summary,
#     aes(xintercept = plus_2sd),
#     linetype = "dashed",
#     linewidth = 0.8
#   ) +
#   geom_vline(
#     data = site_summary,
#     aes(xintercept = minus_2sd),
#     linetype = "dashed",
#     linewidth = 0.8
#   ) +
#   coord_cartesian(
#     xlim = c(x_min, x_max)
#   ) +
#   scale_x_continuous(
#     breaks = seq(x_min, x_max, by = 2 * sd_diff),
#     labels = scales::label_number(accuracy = 0.01)
#   ) +
#   labs(
#     title = paste("Temperature Differences —", site_to_plot),
#     subtitle = "x-axis = ±5 SD, solid line = mean, dashed lines = 2 SD",
#     x = "Temperature difference from previous record (°C)",
#     y = "Count"
#   ) +
#   theme_minimal()

# Create histogram for each site all at once and save as PNG files

# sites <- unique(temp_diff_summary$site_ID)
# 
# for (site_to_plot in sites) {
#   
#   site_diff <- df_temp_diff %>%
#     filter(
#       site_ID == site_to_plot,
#       time_diff_min == 10
#     )
#   
#   site_summary <- temp_diff_summary %>%
#     filter(site_ID == site_to_plot)
#   
#   # Pull summary stats
#   mean_diff <- site_summary$mean_temp_diff
#   sd_diff <- site_summary$sd_temp_diff
#   
#   # Dynamic x-axis limits
#   x_min <- mean_diff - 5 * sd_diff
#   x_max <- mean_diff + 5 * sd_diff
#   
#   # Dynamic bin width
#   binwidth_value <- sd_diff / 4
#   
#   p <- ggplot(site_diff, aes(x = temp_diff)) +
#     geom_histogram(
#       binwidth = binwidth_value,
#       color = "white"
#     ) +
#     geom_vline(
#       xintercept = mean_diff,
#       linetype = "solid",
#       linewidth = 0.8
#     ) +
#     geom_vline(
#       xintercept = c(
#         site_summary$plus_2sd,
#         site_summary$minus_2sd
#       ),
#       linetype = "dashed",
#       linewidth = 0.8
#     ) +
#     geom_vline(
#       xintercept = c(
#         site_summary$plus_4sd,
#         site_summary$minus_4sd
#       ),
#       linetype = "dashed",
#       linewidth = 0.8
#     ) +
#     geom_vline(
#       xintercept = c(
#         site_summary$plus_4sd,
#         site_summary$minus_4sd
#       ),
#       linetype = "dashed",
#       linewidth = 0.8
#     ) +
#     coord_cartesian(
#       xlim = c(x_min, x_max)
#     ) +
#     scale_x_continuous(
#       breaks = seq(x_min, x_max, by = 2 * sd_diff),
#       labels = scales::label_number(accuracy = 0.01)
#     ) +
#     labs(
#       title = paste("Temperature Differences —", site_to_plot),
#       subtitle = "May–Oct, 10-minute intervals only; solid line = mean, dashed lines = ±2 and ±4 SD",
#       x = "Temperature difference from previous record (°C)",
#       y = "Count"
#     ) +
#     theme_minimal()
#   
#   ggsave(
#     filename = paste0(
#       "Temp_Diff_Histogram_",
#       site_to_plot,
#       ".png"
#     ),
#     plot = p,
#     width = 8,
#     height = 6,
#     dpi = 300
#   )
  
# }