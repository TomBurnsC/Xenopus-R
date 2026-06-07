# ==================================================
# Script: 03_feature_engineering.R
#
# Purpose:
# Create derived variables used in
# environmental analyses.
# ==================================================

# Restrict to operational salinity range ----

females_salinity <- females_merged %>%
  filter(Salinity < 850)

# Centre date ----

females_salinity <- females_salinity %>%
  mutate(
    date_c =
      as.numeric(
        DateReported -
          mean(
            DateReported,
            na.rm = TRUE
          )
      )
  )

# Within-between decomposition ----
# Separates within-individual
# salinity effects from
# between-individual effects.

females_salinity <- females_salinity %>%
  group_by(ID) %>%
  mutate(
    Salinity_mean =
      mean(
        Salinity,
        na.rm = TRUE
      ),
    
    Salinity_within =
      Salinity -
      Salinity_mean
  ) %>%
  ungroup()

# Baseline weight categories ----

baseline_weights <- females %>%
  arrange(ID, DateReported) %>%
  group_by(ID) %>%
  slice_head(n = 2) %>%
  summarise(
    baseline_weight =
      mean(
        Weight_before,
        na.rm = TRUE
      ),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs == 2) %>%
  mutate(
    weight_class =
      ntile(
        baseline_weight,
        3
      ),
    weight_class =
      c(
        "low",
        "medium",
        "high"
      )[weight_class]
  )

females_salinity <- females_salinity %>%
  left_join(
    baseline_weights %>%
      select(
        ID,
        baseline_weight,
        weight_class
      ),
    by = "ID"
  )

saveRDS(
  females_salinity,
  here(
    "data",
    "processed",
    "females_salinity.rds"
  )
)
