get_life_tables <- function() {
  require(openxlsx)
  require(data.table)

  url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nltuk198020203.xlsx"

  # Read data
  df <- read.xlsx(
    xlsxFile = url,
    sheet = "2020-2022",
    startRow = 6
  )

  # Convert to data.table
  data.table::setDT(df)

  # Ensure unique column names
  names(df) <- make.unique(names(df))

  # Rename columns: 2 to 6 for "male" and 8 to 12 for "female"
  male_cols <- names(df)[2:6]
  female_cols <- names(df)[8:12]

  setnames(
    df,
    old = c(male_cols, female_cols),
    new = c(paste0(male_cols, "_male"), paste0(gsub("\\.1$", "", female_cols), "_female"))
  )

  df <- df[, .(age, ex_male, ex_female)]

  return(df)
}
