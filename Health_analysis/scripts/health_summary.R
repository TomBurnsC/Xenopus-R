outcome_counts <- health_data %>%
  count(two_month_bin, Outcome)

ggplot(outcome_counts,
       aes(two_month_bin, n, fill = Outcome)) +
  geom_col() +
  theme_minimal()

diagnosis_cols <- health_data[,14:ncol(health_data)]

diagnosis_cols %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything()) %>%
  ggplot(
    aes(
      reorder(name,value),
      value
    )
  ) +
  geom_col() +
  coord_flip()
