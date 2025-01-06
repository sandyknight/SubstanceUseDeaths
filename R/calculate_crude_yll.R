prepare_yll_data <-
  function(substance = "both") {

    switch(substance,
      "both" = {

        alcohol_deaths <-
          merge_alcohol_deaths()

        alcohol_deaths <- # Aggregation FIXME: this should be done in the
          # merge_alcohol_deaths() function.
          alcohol_deaths[, .(count = sum(count)), by = .(sex, age_group)]

        alcohol_deaths[, substance := "Alcohol"]

        drug_deaths <-
          merge_drug_deaths(groups = c("age", "sex"))

        drug_deaths <-
          assign_age_groups(df = drug_deaths, groups = c("sex"))

        drug_deaths[, substance := "Drugs"]

        life_tables <- # FIXME: move this outside of switch
          get_life_tables()

        life_tables <-
          assign_age_groups(life_tables, aggregation_function = mean)

        life_tables <-
          data.table::melt(life_tables,
                           measure.vars = c("ex_female", "ex_male"),
                           variable.name = "sex",
                           value.name = "life_expectancy")

        life_tables[, sex := gsub("ex_", "", sex), ]

        df <-
          data.table::rbindlist(l = list(drug_deaths, alcohol_deaths),
                              use.names = TRUE)

        df <-
          data.table::merge.data.table(df,
                                       life_tables,
                                       by = c("age_group", "sex"))



      },
      "drugs" = {

        drug_deaths <-
          merge_drug_deaths(groups = c("age", "sex"))

        drug_deaths <-
          assign_age_groups(df = drug_deaths, groups = c("sex"))

        drug_deaths[, substance := "Drugs"]

        life_tables <- # FIXME: move this outside of switch
          get_life_tables()

        life_tables <-
          data.table::melt(life_tables,
                           measure.vars = c("ex_female", "ex_male"),
                           variable.name = "sex",
                           value.name = "life_expectancy")

        life_tables[, sex := gsub("ex_", "", sex), ]

        df <-
          data.table::merge.data.table(df,
                                       life_tables,
                                       by = c("age_group", "sex"))

      },
      "alcohol" = {

        alcohol_deaths <-
          merge_alcohol_deaths()

        alcohol_deaths <- # Aggregation FIXME: this should be done in the
          # merge_alcohol_deaths() function.
          alcohol_deaths[, .(count = sum(count)), by = .(sex, age_group)]

        life_tables <- # FIXME: move this outside of switch
          get_life_tables()

        life_tables <-
          data.table::melt(life_tables,
                           measure.vars = c("ex_female", "ex_male"),
                           variable.name = "sex",
                           value.name = "life_expectancy")

        life_tables[, sex := gsub("ex_", "", sex), ]

        df <-
          data.table::merge.data.table(alcohol_deaths,
                                       life_tables,
                                       by = c("age_group", "sex"))

      }
    )

    return(df)
  }

