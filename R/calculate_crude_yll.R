calculate_crude_yll <-
  function() {
    alcohol_deaths <-
      merge_alcohol_deaths()

    drug_deaths <-
      merge_drug_deaths()

    life_tables <-
      get_life_tables()

    print(drug_deaths)
    print(alcohol_deaths)
    print(life_tables)

  }
