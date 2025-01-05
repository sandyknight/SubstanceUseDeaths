#  This file is NDTMS-ONS data linkage. received by email from Stefan and
#  named: 'table1_all deaths_Cocaine version 1.xlsx'

get_drug_poisoning_deaths <- function() {


  if (!file.exists("data/raw/drug_deaths_data.csv")) {
    # Load deaths data from Excel
    df <-
      openxlsx::read.xlsx("data/raw/drugs_deaths_data.xlsx",
        sheet = "table1_all deaths"
      )

    # Clean names
    df <- janitor::clean_names(df)

    # Write data to csv
    data.table::fwrite(df, "data/raw/drug_deaths_data.csv")
  }

  # Return raw data
  df <- data.table::fread("data/raw/drug_deaths_data.csv")

  # Standardise some names
  data.table::setnames(
    df,
    c("ageinyrs", "age_death_grp"),
    c("age", "age_group")
  )

  return(df)
}
