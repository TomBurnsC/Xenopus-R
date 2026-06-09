# ===================================================================================
# EggBound females Analysis 
# ===================================================================================

analysis_data <- females %>%
  filter(!is.na(AgeWhenReported.Weeks.), !is.na(Weight_after)) %>%
  mutate(
    centred_weeks = scale(AgeWhenReported.Weeks., scale = FALSE),
    eggbound = ifelse(ID %in% health_data$ID[health_data$Eggbound == 1], 1, 0)
  ) %>%
  filter(AgeWhenReported.Weeks. < 400) |>
  filter(Weight_after < 20)


analysis_data_interval <- females_interval %>%
  filter(!is.na(AgeWhenReported.Weeks.), !is.na(Weight_after)) %>%
  mutate(
    centred_weeks = scale(AgeWhenReported.Weeks., scale = FALSE),
    eggbound = ifelse(ID %in% health_data$ID[health_data$Eggbound == 1], 1, 0)
  ) 
  

eggbound_sub <- analysis_data %>%
  filter(eggbound == 1)

# ==============================
# Fit growth models
# ==============================

# Population growth slope
m_growth <- lmer(
  Weight_after ~ centred_weeks + I(centred_weeks^2) + (1 | ID),
  data = analysis_data
)

# Eggbound growth slope
m_eggbound <- lmer(
  Weight_after ~ centred_weeks + I(centred_weeks^2) + (1 | ID),
  data = eggbound_sub
)

summary(m_eggbound)
summary(m_growth)

# Interaction slope 
eggbound_interaction <- glmer(
  eggbound ~ centred_weeks * Weight_after + (1|ID),
  family = "binomial",
  data = analysis_data
)

eggbound_interaction <- glmer(
  eggbound ~ Weight_after + (1|ID),
  family = "binomial",
  data = analysis_data
)

summary(eggbound_interaction)

# Eggbinding probability model
m_risk <- glm(
  eggbound ~  Weight_before + I(Weight_before^2),
  family = binomial,
  data = analysis_data
)

m_weight <- lm(Weight_after ~ AgeWhenReported.Weeks., data = analysis_data, na.action = na.exclude)
m_clutch_size <- lm(-Weight_change ~ Weight_before, data = analysis_data, na.action = na.exclude)

analysis_data$weight_resid <- resid(m_weight)
analysis_data$clutch_resid <- resid(m_clutch_size)

eggbound_weight <- glmer(eggbound ~ weight_resid + (1|ID), family = binomial, data = analysis_data)

summary(eggbound_weight)

gam_model <- gam(eggbound ~ s(weight_resid), 
                 family = binomial, 
                 data = analysis_data)

ggplot(analysis_data, aes(x = clutch_resid, y = eggbound)) +
  
  # Raw data (jittered)
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  
  # Logistic regression curve
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE) +
  
  # Labels
  labs(
    x = "Deviance from Expected Clutch Size",
    y = "Likelihood of Eggbinding",
    title = "Eggbinding risk by Clutch size trends"
  ) +
  
  theme_classic()

# ==============================
# Weight deviation from population trajectory
# ==============================

analysis_data <- analysis_data %>%
  mutate(
    expected_weight = predict(m_growth, re.form = NA),
    deviation = Weight_after - expected_weight
  )


analysis_data <- analysis_data %>%
  mutate(
    expected_clutch = predict(m_clutch_size, re.form = NA),
    clutch_deviation = -Weight_change - expected_clutch
  )


# Minimum deviation where eggbinding occurs
risk_boundary <- analysis_data %>%
  filter(eggbound == 1) %>%
  summarise(min_dev = min(deviation)) %>%
  pull(min_dev)


# ==============================
# Prediction grid for plotting
# ==============================

age_seq <- seq(
  min(analysis_data$centred_weeks),
  max(analysis_data$centred_weeks),
  length.out = 200
)

pred_df <- tibble(
  centred_weeks = age_seq
) %>%
  mutate(
    pop_growth = predict(m_growth, newdata = ., re.form = NA),
    eggbound_growth = predict(m_eggbound, newdata = ., re.form = NA),
    risk_boundary = pop_growth + risk_boundary
  )

# ==============================
# Plot growth trajectories
# ==============================

ggplot() +
  
  # individual frog trajectories
  geom_line(
    data = analysis_data,
    aes(centred_weeks, Weight_after, group = ID),
    alpha = 0.2
  ) +
  
  # eggbound observations
  geom_point(
    data = filter(analysis_data, eggbound == 1),
    aes(centred_weeks, Weight_after),
    colour = "red",
    size = 2
  ) +
  
  # population growth curve
  geom_line(
    data = pred_df,
    aes(centred_weeks, pop_growth),
    colour = "blue",
    size = 1.2
  ) +
  
  # eggbound growth curve
  geom_line(
    data = pred_df,
    aes(centred_weeks, eggbound_growth),
    colour = "darkred",
    size = 1.2
  ) +
  
  # risk boundary
  geom_line(
    data = pred_df,
    aes(centred_weeks, risk_boundary),
    linetype = "dashed",
    colour = "red",
    size = 1.2
  ) +
  
  labs(
    x = "Weeks (centred)",
    y = "Weight (g)",
    title = "Growth trajectories and eggbinding risk",
    subtitle = "Dashed red line = minimum deviation where eggbinding occurs"
  ) +
  
  theme_classic()


risk_model <- glm(
  eggbound ~ deviation,
  family = binomial,
  data = analysis_data
)

risk_model_clutch <- glm(
  eggbound ~ clutch_deviation,
  family = binomial,
  data = analysis_data
)

summary(risk_model)
summary(risk_model_clutch)

ggplot(analysis_data, aes(deviation, eggbound)) +
  geom_jitter(height = 0.05, alpha = 0.4) +
  stat_smooth(method = "glm", method.args = list(family="binomial")) +
  theme_classic() 
  

# Extract coefficients
coefs <- coef(risk_model)

# 50% probability threshold
threshold_50 <- -coefs[1] / coefs[2]

threshold_50
