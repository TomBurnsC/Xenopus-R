pred <- ggpredict(
  salinity_weight_class,
  terms = c(
    "Salinity [all]",
    "weight_class"
  )
)

p1 <- ggplot(
  pred,
  aes(
    x = x,
    y = predicted,
    colour = group
  )
) +
  geom_line(size = 1.2)

ggsave(
  here(
    "outputs",
    "figures",
    "salinity_weight_class.png"
  ),
  plot = p1,
  width = 8,
  height = 6
)