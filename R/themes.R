my_theme <- function(dark = FALSE) {
  ggplot2::theme_dark() %+replace%
    #  ggplot2::theme_bw() %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(family = "sans", size = 50),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title.position = "top",
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "black"),
      strip.text = ggplot2::element_text(colour = "white", hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}
