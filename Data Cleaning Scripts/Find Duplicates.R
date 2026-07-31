library(dplyr)
library(lubridate)
library(readr)
library(tidyr)


# Path to your CSV
path <- "C:/Users/zombi/Desktop/Capstone/ENV_DATA_18_25_YAQUINA_11.15.25_DM.csv"
# Read the CSV
df <- read_csv(path, show_col_types = FALSE)

# Create datetime column
df <- df |>
  mutate(datetime = mdy_hms(paste(date, time)))

# Find duplicates by site_ID + datetime
dupes <- df %>%
  group_by(site_ID, datetime) %>%
  filter(n() > 1) %>%
  arrange(site_ID, datetime)

# Write out full duplicate records
write_csv(dupes, "C:/Users/zombi/Desktop/Capstone/Duplicate_Values_Full.csv")

# Summarize number of duplicates per site
dupe_summary <- dupes %>%
  group_by(site_ID) %>%
  summarise(num_dupes = n(), .groups = "drop") %>%
  arrange(desc(num_dupes))

print(dupe_summary)

# How many distinct timestamps are duplicated 

dupe_summary_unique <- dupes %>%
  distinct(site_ID, datetime) %>%
  count(site_ID, name = "num_dupe_timestamps") %>%
  arrange(desc(num_dupe_timestamps))

# Create new csv for duplicate data counts
write_csv(dupe_summary_unique, "C:/Users/zombi/Desktop/Capstone/Dupe_Summary_Unique.csv")