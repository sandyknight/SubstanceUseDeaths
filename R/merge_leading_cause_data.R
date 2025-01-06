merge_leading_cause_data <- function() {
  all_cause_mortality <- get_all_cause_mortality()

  # Identify common age groups of interest
  leading_mortality_age_groups <- unique(all_cause_mortality[["age_group"]])
  leading_mortality_age_groups <- leading_mortality_age_groups[4:7]

  all_cause_mortality <-
    all_cause_mortality[age_group %in% leading_mortality_age_groups, ]

  all_cause_mortality[, age_group := gsub(
    " years| years and over", "",
    gsub(" to ", "-", age_group)
  )]

  all_cause_mortality <-
    all_cause_mortality[, .(age_group, leading_cause, deaths)]

  data.table::setnames(all_cause_mortality,
    old = c("leading_cause", "deaths"),
    new = c("cause_of_death", "count")
  )

  alcohol_deaths <- merge_alcohol_deaths()

  alcohol_deaths <-
    alcohol_deaths[, .(count = sum(count)), by = age_group]

  alcohol_deaths[, cause_of_death := "Alcohol"]

  alcohol_deaths <- alcohol_deaths[, .(age_group, cause_of_death, count)]

  alcohol_deaths <-
    alcohol_deaths[age_group %in% all_cause_mortality[["age_group"]], ]

  drug_deaths <- merge_drug_deaths(groups = "age", categorise = FALSE)

  drug_deaths <-
    assign_age_groups(drug_deaths, aggregation_function = sum)

  drug_deaths <-
    drug_deaths[, .(count = sum(count)), by = age_group]

  drug_deaths[, cause_of_death := "Drugs"]

  drug_deaths <- drug_deaths[, .(age_group, cause_of_death, count)]

  drug_deaths <-
    drug_deaths[age_group %in% all_cause_mortality[["age_group"]], ]

  df <- data.table::rbindlist(l = list(all_cause_mortality,
                                       drug_deaths,
                                       alcohol_deaths))

  return(df)


}
