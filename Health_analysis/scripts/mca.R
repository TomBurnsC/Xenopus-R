diagnosis_cols_factored <- diagnosis_cols %>%
  mutate(
    across(
      everything(),
      ~factor(
        .x,
        levels = c(0,1),
        labels = c("Absent","Present")
      )
    )
  )

mca <- MCA(
  diagnosis_cols_factored,
  graph = FALSE
)

fviz_mca_ind(mca)
fviz_mca_var(mca)

fviz_contrib(
  mca,
  choice = "var",
  axes = 1
)

clusters <- HCPC(mca)

saveRDS(
  mca,
  "outputs/models/mca.rds"
)
