females <- read.csv("data/raw/females_df_bulk.csv")
males   <- read.csv("data/raw/males_df_bulk.csv")

full_data <- bind_rows(females, males)

health_data <- read.csv("data/raw/Health_categorised2.csv")

health_data$Issue.Date <- dmy(health_data$Issue.Date)

health_data <- health_data %>%
  mutate(
    two_month_bin = floor_date(Issue.Date, "2 months")
  )
