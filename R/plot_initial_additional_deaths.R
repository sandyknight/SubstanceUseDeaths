plot_initial_additional <- function(df, substance = "both") {
  require(ggplot2)


  switch(substance,
    "drugs" = {
      df <- df[substance == "Drugs", ]
    },
    "alcohol" = {
      df <- df[substance == "Alcohol", ]
    },
    "both" = {
      df
    }
  )

  plot_title <-
    switch(substance,
      "drugs"   = "Deaths related to drug use",
      "alcohol" = "Deaths related to alcohol use",
      "both"    = "Deaths related to substance use"
    )

  df2 <- df[, .(count = sum(count)), by = .(substance)]

  drugs_min <- 100

  drugs_max <- df2[substance == "Drugs", ][["count"]]

  alcohol_min <- drugs_max + (0.05 * drugs_max)

  alcohol_max <- df2[substance == "Alcohol", ][["count"]] + (0.98 * drugs_max)

  cumulative <- cumsum(df[["count"]])

  p <-
    ggplot(df, aes(x = 1.5, y = count)) +
    geom_col(aes(group = rev(category), fill = type),
      width = 0.05,
      alpha = 0.8,
      colour = "black"
    ) +
    hrbrthemes::theme_ipsum_rc(
      base_size = 10,
      plot_title_size = 10,
      grid = FALSE,
      subtitle_size = 10,
      axis = "xy",
      axis_col = "black"
    ) +
    geom_text(
      aes(
        x = 1.5,
        label = scales::comma(count)
      ),
      position = position_stack(vjust = 0.5),
      size = 3,
      colour = "black"
    ) +
    geom_text(aes(x = 1.545, label = category),
      size = 3,
      position = position_stack(vjust = 0.5),
      colour = "black",
      hjust = 0
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(
      legend.position = "bottom",
      legend.justification = "left",
      axis.text.x = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    expand_limits(x = c(1, 4)) +
    labs(
      y = NULL,
      x = NULL,
      fill = NULL,
      title = plot_title,
      subtitle = "England, 2021",
      caption = "Sources:\nAlcohol-specific deaths by sex, age group and individual cause of death (ONS 2022)\nDeaths related to drug poisoning, England and Wales (ONS 2024)\nNDTMS-ONS linked mortality dataset (EAT/NDTMS 2024)
  "
    )

  if (substance == "both") {
    p <-
      p +
      annotate("segment",
        x = 1.46,
        xend = 1.46,
        y = alcohol_min,
        yend = alcohol_max,
        linewidth = 0.25
      ) +
      annotate("segment",
        x = 1.46,
        xend = 1.4625,
        y = alcohol_max,
        yend = alcohol_max
      ) +
      annotate("segment",
        x = 1.46,
        xend = 1.4625,
        y = alcohol_min,
        yend = alcohol_min
      ) +
      annotate("segment",
        x = 1.46,
        xend = 1.46,
        y = 100,
        yend = drugs_max,
        linewidth = 0.25
      ) +
      annotate("segment",
        x = 1.46,
        xend = 1.4625,
        y = drugs_max,
        yend = drugs_max
      ) +
      annotate("segment",
        x = 1.46,
        xend = 1.4625,
        y = drugs_min,
        yend = drugs_min
      ) +
      annotate("label",
        y = alcohol_max - (df2[substance == "Alcohol"][["count"]] / 2.2),
        x = 1.42,
        colour = "black",
        label = paste("Alcohol",
          scales::comma(df2[substance == "Alcohol"][["count"]]),
          sep = "\n"
        ),
        size = 3
      ) +
      annotate("label",
        y = drugs_max - (drugs_max / 2.2),
        x = 1.43,
        size = 3,
        colour = "black",
        label = paste("Drugs",
          scales::comma(df2[substance == "Drugs"][["count"]]),
          sep = "\n"
        )
      )
  }


  p <-
    p +
    geom_segment(
      y = sum(df[["count"]]),
      yend = sum(df[["count"]]),
      x = 1.475,
      xend = 1.3515,
      linetype = 2
    ) +
    annotate("segment",
      x = 1.535,
      xend = 1.535,
      y = 100,
      yend = cumulative[1] * 0.95,
      linewidth = 0.4,
      arrow = arrow(
        angle = 90,
        length = unit(0.1, "cm"),
        ends = "both"
      )
    ) +
    annotate("segment",
      x = 1.535,
      xend = 1.535,
      y = cumulative[1] * 1.05,
      yend = cumulative[2] * 0.95,
      linewidth = 0.4,
      arrow = arrow(
        angle = 90,
        length = unit(0.1, "cm"),
        ends = "both"
      )
    ) +
    annotate("segment",
      x = 1.535,
      xend = 1.535,
      y = cumulative[2],
      yend = cumulative[3],
      linewidth = 0.4,
      arrow = arrow(
        angle = 90,
        length = unit(0.1, "cm"),
        ends = "both"
      )
    ) +
    annotate("segment",
      x = 1.535,
      xend = 1.535,
      y = cumulative[3] * 1.05,
      yend = cumulative[4],
      linewidth = 0.4,
      arrow = arrow(
        angle = 90,
        length = unit(0.1, "cm"),
        ends = "both"
      )
    ) +
    annotate("segment",
      x = 1.535,
      xend = 1.535,
      y = cumulative[4] * 1.01,
      yend = cumulative[5],
      linewidth = 0.4,
      arrow = arrow(
        angle = 90,
        length = unit(0.1, "cm"),
        ends = "both"
      )
    )
  p <-
    p +
    scale_fill_dhsc() +
    annotate("text",
      label = scales::comma(sum(df[["count"]])),
      size = 3.5,
      x = 1.38,
      y = sum(df[["count"]]) - 350
    ) +
    coord_cartesian(
      xlim = c(1.38, 2),
      ylim = c(0, 15e03),
      clip = "off"
    )
  p
}
## png("plots/initial-and-additional-deaths.png",
##   height = 30,
##   width = 30,
##   units = "cm",
##   res = 200
## )
## p
## dev.off()
