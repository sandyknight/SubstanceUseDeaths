plot_other_death_causes <- function(df) {
  df <- df[treatment_status != "Died one or more years following discharge", ]

  df <- df[death_cause != "Alcohol-specific death", ]

  df[, substance := data.table::fifelse(
    drug_group == "alcohol only",
    "Alcohol",
    "Drugs"
  )]

  df2 <- df[, .(count = sum(count)), by = .(death_cause)]

  data.table::setorder(df2, count)

  df <- df[, .(count = sum(count)), by = .(substance, death_cause)]

  df[, death_cause := factor(death_cause, levels = df2[["death_cause"]])]

  ggplot(df, aes(y = death_cause, x = count)) +
    geom_col(aes(fill = substance),
             colour = "black",
             alpha = 0.8,
             width = 0.7) +
    hrbrthemes::theme_ipsum_rc(base_size = 10,
                               plot_title_size = 10,
                               subtitle_size = 10) +
    scale_fill_dhsc() +
    labs(title = "Causes of death other than drug poisoning or alcohol-specific death",
         subtitle = "England 2021",
         x = "Deaths (n)",
         y = NULL,
         caption = ) +
    theme(legend.position = "bottom",
          legend.justification = "left",
          plot.title.position = "plot",
          plot.caption.position = "plot")
}
