library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Path to CSV
path <- "C:/Users/zombi/Desktop/Capstone/ENV_DATA_18_25_YAQUINA_11.15.25_DM.csv"

# Sanity check: does the file exist?
file.exists(path)   # should return TRUE

# Read the CSV
dat <- read_csv(path, show_col_types = FALSE)

# parse and join "date" and "time" columns into new column "datetime"
dat <- dat |>
  mutate(
    datetime = mdy_hms(paste(date, time)), # parses "05/01/2022 00:00" etc.
  )

# Peek at the data
print(dim(dat))   # number of rows and columns
print(head(dat))  # first few rows

# Calculate hourly mean per site
dat_hourly <- dat |>
  mutate(
    hour = floor_date(datetime, "hour"),
    ym   = floor_date(datetime, "month")   # month bin, same for all rows in that hour
  ) |>
  group_by(site_ID, hour, ym) |>
  summarise(
    temp = mean(temp, na.rm = TRUE),       # rename to temp for downstream consistency
    .groups = "drop"
  ) |>
  rename(datetime = hour)

print(head(dat_hourly))

# Bookend months to check
start_month <- ymd("2018-05-01")
end_month   <- ymd("2024-11-01")

dat_hourly <- dat_hourly |>
  filter(datetime >= start_month,
         datetime < (end_month %m+% months(1)))  # up to Dec 1, 2024

# Count available water temp records per site per month 
df_temp <- dat_hourly |>
  filter(!is.na(temp))
obs_counts <- df_temp |>
  group_by(site_ID, ym) |>
  summarise(
    n_records = n(),          # number of hourly means present
    .groups = "drop"
  )

# Build site x month grid
all_sites  <- sort(unique(dat_hourly$site_ID))
all_months <- seq(start_month, end_month, by = "month")

site_month_grid <- expand_grid(
  site_ID = all_sites,
  ym      = all_months
) |>
  mutate(
    days_in_mo     = days_in_month(ym),
    possible_hours = days_in_mo * 24L
  )

# Calculate coverage
coverage_summary <- site_month_grid |>
  left_join(obs_counts, by = c("site_ID", "ym")) |>
  mutate(
    n_records         = replace_na(n_records, 0L),
    coverage_fraction = n_records / possible_hours,
    coverage_percent  = 100 * coverage_fraction,
    has_data          = n_records > 0
  ) |>
  arrange(site_ID, ym)

print(coverage_summary)

# filter for months with at least 50% data and may through november
coverage_filtered <- coverage_summary |>
  filter(
    month(ym) %in% 5:11,            # keep only May–Nov
    coverage_fraction >= 0.50       # keep only months with ≥ 50% coverage
  ) |>
  arrange(site_ID, ym)

# Make wide table
coverage_wide <- coverage_filtered |>
  mutate(ym = format(ym, "%Y-%m")) |>  # nice strings for month labels
  select(ym, site_ID, coverage_percent) |>
  pivot_wider(
    names_from = site_ID,
    values_from = coverage_percent,
    values_fill = 0
  )

# Display year month chronologically in wide table
coverage_wide <- coverage_wide |>
  mutate(ym_date = as.Date(paste0(ym, "-01"))) |>
  arrange(ym_date) |>
  select(-ym_date)


# Show months chronologically
# Convert ym to Date
coverage_wide$ym_date <- as.Date(paste0(coverage_wide$ym, "-01"))
# Sort chronologically
coverage_wide <- coverage_wide[order(coverage_wide$ym_date), ]

# Save the labels before dropping columns
row_labels <- coverage_wide$ym

# Move ym to rownames
rownames(coverage_wide) <- coverage_wide$ym
coverage_wide$ym <- NULL
coverage_wide$ym_date <- NULL

# Convert to matrix
coverage_matrix <- as.matrix(coverage_wide)

heatmap(coverage_matrix, Rowv = NA, Colv = NA, scale = "none", labRow = row_labels)