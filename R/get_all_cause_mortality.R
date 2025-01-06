get_all_cause_mortality <-
  function() {
    # Use cached file if it exists
    if (file.exists("data/processed/ons_leading_mortality_causes.csv")) {

      df <- data.table::fread("data/processed/ons_leading_mortality_causes.csv")

    } else {

      # Download and process mortality data from ONS
      df <-
        openxlsx::read.xlsx(
          xlsxFile = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2022/dr2022corrected.xlsx",
          sheet = "10b",
          rows = c(6:55)
        )

      df <- data.table::as.data.table(df)

      data.table::setnames(df, new = janitor::make_clean_names)

      df[, percentage_of_all_deaths_percent :=
             percentage_of_all_deaths_percent / 100]

      data.table::fwrite(df, "data/processed/ons_leading_mortality_causes.csv")

    }

    return(df)
  }
