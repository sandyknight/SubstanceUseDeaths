plot_yll <- function(df, estimate = "adjusted") {
  switch(estimate,
    "crude" = {
      p <-
        ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = age_group,
            y = crude_estimate
          )
        ) +
        ggplot2::labs(
          subtitle = "Unadjusted estimate\nEngland 2021",
          title = paste(
            "Years of life lost (YLL) related to substance use\nTotal YLL:",
            scales::comma(sum(df[["crude_estimate"]]))
          )
        )
    },
    "adjusted" = {
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = age_group,
          y = adjusted_estimate
        )
      ) +
        ggplot2::labs(
          subtitle = "Adjusted, with discounting and age-weighting
England 2021",
          title = paste(
            "Years of life lost (YLL) related to substance use\nTotal YLL:",
            scales::comma(sum(df[["adjusted_estimate"]]))
          )
        )
    }
  )



  p +
    ggplot2::geom_col(ggplot2::aes(fill = substance), colour = "black") +
    tinythemes::theme_ipsum_rc(
      base_size = 10,
      plot_title_size = 10,
      grid = FALSE,
      subtitle_size = 10,
      axis = "xy",
      axis_col = "black"
    ) +
    scale_fill_dhsc() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "left",
      axis.text.x = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    ggplot2::labs(
      x = "Age group",
      y = "YLL",
      caption = "Sources:\nNDTMS-ONS linked mortality dataset (EAT/NDTMS 2024)
National life tables: England, 2020-2022 (ONS)\nChudasama, Y.V., Khunti, K., Gillies, C.L., Dhalwani, N.N., Davies, M.J., Yates, T., & Zaccardi, F. (2022). Estimates of years of life lost depended on the method used: tutorial and comparative investigation. Journal of Clinical Epidemiology, 150, pp. 42–50. Available at: https://doi.org/10.1016/j.jclinepi.2022.06.012 [Accessed 6 Nov. 2024].
  "
    )
}
