create_prediction_grid <- function(
    data,
    age_col,
    n = 200
){

  age_seq <- seq(
    min(data[[age_col]], na.rm=TRUE),
    max(data[[age_col]], na.rm=TRUE),
    length.out = n
  )

  mean_age <- mean(
    data[[age_col]],
    na.rm = TRUE
  )

  tibble(
    AgeWhenReported.Weeks. = age_seq,
    centred_weeks = age_seq - mean_age
  )
}


fit_growth_model <- function(data){

  lmer(
    Weight_after ~
      centred_weeks +
      (1|ID),
    data = data
  )

}


plot_growth_comparison <- function(
    data,
    pred_df,
    condition_var,
    title
){

  ggplot() +

    geom_ribbon(
      data = pred_df,
      aes(
        AgeWhenReported.Weeks.,
        ymin=lwr,
        ymax=upr
      ),
      alpha=0.3
    ) +

    geom_line(
      data = pred_df,
      aes(
        AgeWhenReported.Weeks.,
        fit_all
      ),
      linewidth=1.2
    ) +

    geom_point(
      data = data,
      aes(
        AgeWhenReported.Weeks.,
        Weight_after,
        colour=.data[[condition_var]]
      )
    ) +

    labs(title=title)

}
