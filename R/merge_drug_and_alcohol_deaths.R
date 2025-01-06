merge_drug_and_alcohol_deaths <-
  function() {

    alcohol_deaths <-
      merge_alcohol_deaths()

    alcohol_deaths[
      ,
      category :=
        data.table::fifelse(
          death_cause == "Alcohol-specific death",
          "Alcohol-specific deaths (ONS)",
          "Other cause of death, in alcohol treatment
or within a year of discharge (NDTMS)"
        )
    ]


    alcohol_deaths <-
      alcohol_deaths[, .(count = sum(count)), by = .(category)]

    alcohol_deaths[, substance := "Alcohol"]

    drug_deaths <-
      merge_drug_deaths()

    drug_deaths[, substance := "Drugs"]

    df <-
      data.table::rbindlist(l = list(drug_deaths, alcohol_deaths))

    df <-
      df[grep(pattern = "one or more years", x = category, invert = TRUE)]

    df[, category := forcats::as_factor(category)]

    df[, type := data.table::fifelse(grepl("NDTMS", category),
                                     "Additional deaths",
                                     "Initial deaths")]

    df[, type := forcats::as_factor(type)]


    return(df)
  }
