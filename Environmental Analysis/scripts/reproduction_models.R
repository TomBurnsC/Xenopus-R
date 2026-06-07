egg_model <- glmer(
  Eggs ~
    scale(Salts) +
    scale(DateReported) +
    (1|ID),
  family = binomial,
  data = females_salinity
)

saveRDS(
  egg_model,
  here(
    "outputs",
    "models",
    "egg_model.rds"
  )
)