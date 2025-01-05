#' Process drug poisoning data
#'
#' Filters and groups the data and aggregates based on user-defined parameters: 'date of' years, and grouping columns (age, sex, area, and drug group, and misuse).
#'
#' Date of (`date_of = "occurrence"`)
#' The data has both the year-month of death ("occurrence") and of registration of the death. Registration is preferable when looking for trends that include the latest dates available due to registration lag, In most other contexts occurrence is preferable.
#'
#' Misuse (`misuse_only = TRUE`)
#' Each observation is flagged as being recorded by the ONS as "related to drug misuse" or not. Set `misuse = TRUE` to filter out deaths that the ONS did not record as related to drug misuse.
#'
#' @param data a data frame, the output of get_drug_poisoning_deaths()
#' @param date_of Either "occurrence" (year of death) or "registration" (year of registration).
#' @param years An integer vector of years to include in the analysis.
#' @param drug_group a string vector. "all" group by drug group, "total" total deaths drug group only, can also be a vector of any of the drug groups in the column `drug_group`
#' @param group_by Grouping variable, any of: "area", "age", "sex".
#' @param misuse_only TRUE filters deaths not recorded as related to drug misuse.
#' @return A data frame with a count of drug death occurrences or registrations, aggergated by the grouping variables and filtered by the selected years.
process_drug_poisoning_data <- function(data,
                                        date_of = "occurrence",
                                        years = NULL,
                                        groups = NULL,
                                        drug_groups = "total",
                                        misuse_only = TRUE) {
  require(data.table)

  # Decide which date variable to use
  date_of_var <- switch(date_of,
    "occurrence"   = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurrence' or 'registration' are valid options")
  )

  switch(
    # If 'drug_groups' is "total", keep only 'Total Deaths'
    drug_groups,
    "total" = {
      data <- data[drug_group == "Total Deaths"]
    },

    # If 'drug_groups' is "all", do nothing (i.e., keep all rows)
    "all" = {
      # No filtering, but keep group_by as-is
      groups <- c(groups, "drug_group")
    },

    # If `drug_groups` is either a custom string or vector of drug groups
    {
      # If not "total" or "all", we assume it's either a vector
      # of group names or a single string (like "Opioids")
      data <- data[drug_group %in% drug_groups]
      groups <- c(groups, "drug_group")
    }
  )

  # Filter by the selected years
  if (!is.null(years)) {
    data <- data[get(date_of_var) %in% years]
  }

  # Filter by misuse flag
  if (isTRUE(misuse_only)) {
    data <- data[drug_misuse_combined == 1]
  }

  # Ensure all necessary grouping variables are included
  groups <- c(groups, date_of_var)
  groups <- unique(groups)

  # Aggregate data by the grouping variables
  result <- data[, .(count = .N), by = groups]

  return(result)
}
