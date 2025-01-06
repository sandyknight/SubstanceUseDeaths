#' Assign age groups
#'
#' Since the publicly available alcohol-specific deaths data is only available
#' by age with age groups we have to group the single-year of age data in the
#' same way.
#'
#' @param df a data table with a single-year-of-age column called `age`
#' @param aggregation.function "sum" or "mean", use mean for life expectancy
#' @return a data table summarised by the ONS age groups in a column called
#' `age_group`

assign_age_groups <-
  function(df, groups = NULL, aggregation_function = sum) {

    ## Make a lookup table for the ONS age groups
    age_group_lkp <- data.table(
      age_group = c(
        "<1",
        "01-04",
        "05-09",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85-89",
        "90+"
      ),
      min_age = c(-Inf, 1, seq(5, 85, 5), 90),
      max_age = c(0, seq(4, 89, 5), Inf)
    )

    # Now we can use the lookup table to do a non-equi join
    df <-
      df[age_group_lkp,
         on = .(age >= min_age, age <= max_age),
         age_group := i.age_group]

    # Handle user groups

    if (is.null(groups)) {
      groups <- "age_group"
    } else {
      groups <- c("age_group", groups)
    }

    # Aggregate
    df <- df[, lapply(.SD, aggregation_function),
             by = groups, .SDcols = is.numeric]
    df[, age := NULL, ]

    return(df)
  }

midpoint_ages <- function() {

  age_group_lkp <- data.table(
    age_group = c(
      "<1",
      "01-04",
      "05-09",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85-89",
      "90+"
    ),
    min_age = c(1, 1, seq(5, 85, 5), 90),
    max_age = c(0, seq(4, 89, 5), 100)
  )

  age_group_lkp[, midpoint := (min_age + max_age) / 2]

  age_group_lkp[, .(age_group, midpoint)]

  return(age_group_lkp)

}
