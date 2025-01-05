get_ons_drug_poisoning_data <-
  function() {
    url <-
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable/current/2023registrations.xlsx"

    df <-
      openxlsx::read.xlsx(
        url,
        sheet = "Table 1",
        startRow = 4,
        sep.names = "_",
        fillMergedCells = TRUE,
        skipEmptyCols = TRUE,
        check.names = TRUE,
        cols = c(1, 2, 5, 9)
      )

    data.table::setDT(df)

    data.table::setnames(
      df,
      c("sex", "year", "all_drug_poisoning", "drug_misuse")
    )

    df <- df[3:97, ]

    df[, sex := zoo::na.locf(sex), ]

    df <- df[sex == "Persons", ][!is.na(year), ]

    return(df)
  }
