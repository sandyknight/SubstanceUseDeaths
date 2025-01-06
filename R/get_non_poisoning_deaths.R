#' Load NDTMS-ONS mortality linked dataset

get_non_poisoning_deaths <-
  function() {
    
    if (file.exists("data/processed/non_poisoning_deaths_data.csv")) {
      df <- data.table::fread(
        "data/processed/non_poisoning_deaths_data.csv"
      )
    } else {
      require(openxlsx)

      df <-
        openxlsx::read.xlsx("data/raw/non_poisoning_deaths_data.xlsx",
          sheet = "NDTMS_ONS",
          detectDates = TRUE
        ) |>
        janitor::clean_names()

      data.table::setDT(df)

      df[, period := base::as.Date(period, origin = "1899-12-30"), ]

      df <- df[death_cause != "Drug poisoning", ]

      data.table::fwrite(
        df,
        "data/processed/non_poisoning_deaths_data.csv"
      )

      df <-
        data.table::fread(
          "data/processed/non_poisoning_deaths_data.csv"
        )
    }

    # Remove country-level df
    df <- df[geography == "LA", ]

    # Re-format periods
    df[
      , year := stringr::str_extract(period_range, "\\d{4}")
    ][
      , year := as.integer(year)
    ]

    # Select columns
    df <-
      df[, .(
        year,
        area_code,
        area_name,
        death_cause,
        age,
        agegrp,
        sex,
        drug_group,
        treatment_status,
        count
      )]

    # Rename age group column
    data.table::setnames(df, "agegrp", "age_group")


    return(df)
  }
