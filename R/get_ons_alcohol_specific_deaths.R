get_ons_alcohol_specific_deaths <-
  function() {
    

    if (!file.exists("data/processed/ons_alcohol_specific_deaths.csv")) {
      url <-
        "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsbysexagegroupandindividualcauseofdeath/current/deathsbyindividualcause.xlsx"

      df <-
        openxlsx::read.xlsx(
          xlsxFile = url,
          rows = c(5:50),
          cols = c(1:24),
          colNames = TRUE,
          sheet = "Table 2"
        )

      data.table::setDT(df)

      df <-
        data.table::melt(
          df,
          measure.vars = data.table::patterns(
            ## Melt age group columns by regex
            "<\\d|\\d\\d-\\d\\d|\\d\\d\\+"
          ) ## Column names handles in next line
        )

      data.table::setnames(
        df,
        c(
          "year",
          "sex",
          "icd10",
          "death_cause",
          "age_group",
          "count"
        )
      )

      ## Get count of age group and sex and exclude "persons" value in sex column
      df <-
        df[, .(count = sum(count)), by = c("sex", "age_group")][sex != "Persons"]


      # Standardise the sex column
      df[, sex := gsub("s$", "", sex)] # Remove trailing 's'
      df[, sex := tolower(sex), ] # Convert to lower case

      data.table::fwrite(df, "data/processed/ons_alcohol_specific_deaths.csv")
    } else {
      df <- data.table::fread("data/processed/ons_alcohol_specific_deaths.csv")
    }

    return(df)
  }
