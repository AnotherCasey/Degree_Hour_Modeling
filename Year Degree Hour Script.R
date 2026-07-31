library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Path to CSV
path <- "C:/Users/zombi/Desktop/Capstone/ENV_DATA_18_25_YAQUINA_11.15.25_DM.csv"

# Sanity check: does the file exist?
file.exists(path)   # should return TRUE

# Read the CSV
dat <- read_csv(path, show_col_types = FALSE)

## Parameters
target_year <- 2023L
thr <- c(15, 20) # Set thresholds for degree hour calculations


# parse and join "date" and "Time" columns into new column "datetime"
dat <- dat |>
  mutate(
    datetime = mdy_hms(paste(date, time))  # parses "05/01/2022 00:00" etc.
  )

# Peek at the data
print(dim(dat))   # number of rows and columns
print(head(dat))  # first few rows

# Filter for site and time range
dat_seasonal <- dat |>
  filter(year(datetime) == target_year) |>
  mutate(
    yr           = year(datetime),
    season_start = ymd(paste(yr, "05-01")),
    season_end   = ymd(paste(yr, "10-01"))
  ) |>
  # keep only rows within time range for that row's year
  filter(datetime >= season_start, datetime <= season_end) |>
  select(site_ID, yr, datetime, temp)


# Calculate hourly mean per site
dat_hourly <- dat_seasonal |>
  mutate(hour = floor_date(datetime, "hour")) |>
  group_by(site_ID, hour) |>
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    yr       = target_year,
    datetime = hour
  ) |>
  select(site_ID, yr, datetime, mean_temp)


# Calculate degree hours per parameters
deg_2023 <- dat_hourly |>
  crossing(threshold = thr) |>   # Make a copy of each row per threshold
  arrange(site_ID, threshold, yr, datetime) |>
  group_by(site_ID, threshold, yr) |>
  mutate(
    degree_hours       = pmax(mean_temp - threshold, 0) * 1,  # 1-hour interval
    total_degree_hours = cumsum(degree_hours)
  ) |>
  ungroup()

# Full time series with cumulative per year & threshold (for plotting/inspection)
deg_2023

# Yearly totals per site and threshold (table summary)
site_totals_2023 <- deg_2023 |>
  group_by(site_ID, threshold, yr) |>
  summarise(
    start_datetime      = min(datetime, na.rm = TRUE),
    end_datetime        = max(datetime, na.rm = TRUE),
    n_valid_points     = sum(is.finite(degree_hours)),  # counts non-NA/NaN
    total_degree_hours = if_else(
      n_valid_points > 0,
      sum(degree_hours, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

site_totals_2023

# Plot degree_hours for each year of site
# 1) Build cumulative, plus a gap flag and segment id
deg_plot_2023 <- deg_2023 |>
  arrange(site_ID, threshold, datetime) |>
  group_by(threshold, site_ID) |>
  mutate(
    gap = is.na(degree_hours),              # TRUE where hourly mean was missing
    seg = cumsum(gap),                      # increments at each NA -> new segment
    dh_for_cum = replace_na(degree_hours, 0),
    total_degree_hours = cumsum(pmax(dh_for_cum, 0))
  ) |>
  ungroup()

# 2) Normalize x to a reference year so months align 
TZ <- "America/Los_Angeles"; ref_year <- 2000L
plot_df <- deg_plot_2023 |>
  mutate(season_time = make_datetime(
    ref_year,
    month(datetime), day(datetime),
    hour(datetime), minute(datetime), second(datetime),
    tz = TZ
  )) |>
  filter(month(season_time) >= 5 &
           (month(season_time) < 10 | (month(season_time) == 10 & day(season_time) == 1)))

# 3) Plot: drop the gap rows and group by (yr, threshold, seg) so lines break at gaps
ggplot(plot_df |> filter(!gap),
       aes(season_time, total_degree_hours,
           group = interaction(site_ID, threshold, seg),
           color = factor(site_ID))) +
  geom_line() +
  facet_wrap(~ threshold, ncol = 1, scales = "free_y") +
  scale_x_datetime(breaks = scales::date_breaks("1 month"),
                   labels = scales::date_format("%b")) +
  labs(x = "Month", y = "Cumulative degree hours", color = "Site") +
  theme_minimal()

write_csv(site_totals_2023, "C:/Users/zombi/Desktop/Capstone/Site_DHTotals_2023.csv")
