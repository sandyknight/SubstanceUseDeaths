#' Summarise non-poisoning deaths data
#'
#' @param data A data frame, the output of get_non_poisoning_deaths()
#' @param years A numeric vector of years to select
#' @param groups A string vector:
#' area, death_cause, age, age_group, sex, treatment_status
summarise_non_poisoning_deaths <-
  function(data, years = NULL, groups) {
    require(data.table)

    if (!is.null(years)) {
      data <- data[year %in% years, ]
    }

    groups <- c("year", groups)

    data <- data[, .(count = .N), by = groups]

    return(data)
  }
