merge_drug_deaths <-
  function(groups = NULL, categorise = TRUE) {
    if (is.null(groups)) {
      groups_poisoning <- c("drug_misuse_combined", "treatment_status")
    } else {
      groups_poisoning <- c("drug_misuse_combined", "treatment_status", groups)
    }

    poisoning_deaths <-
      process_drug_poisoning_data(
        data = get_drug_poisoning_deaths(),
        years = c(2022),
        misuse_only = TRUE,
        groups = groups_poisoning
      )

    poisoning_deaths[
      ,
      category := data.table::fcase(
        drug_misuse_combined == 1,
        "Poisoning deaths classified as related to drug misuse (ONS)",
        drug_misuse_combined == 0 & treatment_status != "no match with NDTMS",
        "Poisoning deaths not classified as misuse (ONS),
 treatment record (NDTMS)",
        drug_misuse_combined == 0 & treatment_status == "no match with NDTMS",
        "Poisoning deaths not classified as misuse, no treatment record (NDTMS)"
      )
    ]

    poisoning_deaths <-
      poisoning_deaths[, .(count = sum(count)), by = c("category", groups)]

    non_poisoning_deaths <-
      process_non_poisoning_data(
        get_non_poisoning_deaths(),
        years = c(2022),
        substance_group = "drugs",
        by_treatment_status = TRUE,
        groups = groups
      )

    non_poisoning_deaths <-
      non_poisoning_deaths[, category := paste(gsub(
        "Died ",
        "Non-poisoning deaths ",
        treatment_status
      ), "(NDTMS)")]

    if (is.null(groups)) {
      select_cols <- c("category", "count")
    } else {
      select_cols <- c("category", groups, "count")
    }

    non_poisoning_deaths <-
      non_poisoning_deaths[, ..select_cols, ]

    df <-
      data.table::rbindlist(l = list(
        poisoning_deaths,
        non_poisoning_deaths
      ), use.names = TRUE)

    if ("sex" %in% colnames(df)) {
      df[, sex := data.table::fcase(sex == "M", "male",
           sex == "F", "female",
           default = sex
         )]
    }

    if (isFALSE(categorise)) {
      if (is.null(groups)) {
        df[, .(count = sum(count))]
      } else {
        df[, .(count = sum(count)), by = groups]
      }
    } else {
      groups <- c("category", groups)
      df[, .(count = sum(count)), by = groups]
    }
  }
