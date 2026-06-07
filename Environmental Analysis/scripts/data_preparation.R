# ==================================================
# Script: 02_data_preparation.R
#
# Purpose:
# Prepare environmental and colony datasets
# for downstream analyses.
# ==================================================

# Date conversion ----

females$DateReported <- dmy(females$DateReported)
males$DateReported <- dmy(males$DateReported)

environmental$Date <- dmy(environmental$Date)

health_data$Issue.Date <- dmy(
  health_data$Issue.Date
)

salts$Date <- dmy(salts$Date)

# Merge salinity data ----

environmental <- environmental %>%
  left_join(salts, by = "Date")

# Environmental variables ----

env_vars <- c(
  "Ammonia",
  "No2",
  "DO",
  "No3",
  "pH",
  "Salinity"
)

environmental <- environmental %>%
  mutate(
    across(
      all_of(env_vars),
      ~ as.numeric(as.character(.))
    )
  ) %>%
  filter(!is.na(Date))

# Weekly aggregation function ----

weekly_bin <- function(df, date_col){
  
  df %>%
    mutate(
      week = floor_date(
        {{date_col}},
        "week",
        week_start = 1
      )
    )
}

females <- weekly_bin(
  females,
  DateReported
)

males <- weekly_bin(
  males,
  DateReported
)

environmental <- weekly_bin(
  environmental,
  Date
)

health_data <- weekly_bin(
  health_data,
  Issue.Date
)

# Weekly environmental averages ----

env_weekly <- environmental %>%
  group_by(week) %>%
  summarise(
    across(
      all_of(env_vars),
      mean,
      na.rm = TRUE
    ),
    Salts = mean(
      Salts,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Merge environmental data ----

females_merged <- females %>%
  left_join(
    env_weekly,
    by = "week",
    relationship = "many-to-many"
  )

males_merged <- males %>%
  left_join(
    env_weekly,
    by = "week",
    relationship = "many-to-many"
  )

merged_data <- bind_rows(
  females_merged,
  males_merged
)

merged_health <- health_data %>%
  left_join(
    env_weekly,
    by = "week",
    relationship = "many-to-many"
  )

write_rds(
  merged_data,
  here(
    "data",
    "processed",
    "merged_data.rds"
  )
)