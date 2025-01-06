calculate_yll <-
  # FIXME this should instead calculate YLL before merging with the ONS
  # data rather than after.
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
          merge_drug_deaths(groups = c("age", "sex"), categorise = FALSE)

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

        df <-
          merge_drug_deaths(groups = c("age", "sex"), categorise = FALSE)

        life_tables <- # FIXME: move this outside of switch
          get_life_tables()

        life_tables <-
          data.table::melt(life_tables,
                           measure.vars = c("ex_female", "ex_male"),
                           variable.name = "sex",
                           value.name = "life_expectancy",
                           variable.factor = FALSE)

        life_tables[, c("sex", "age") :=
                      list(gsub("ex_", "", sex),
                           as.integer(age)), ]

        df <-
          data.table::merge.data.table(df,
                                       life_tables,
                                       by = c("age", "sex"))

      },
      "alcohol" = {

        df <-
          merge_alcohol_deaths()

        df <- # Aggregation FIXME: this should be done in the
          # merge_alcohol_deaths() function.
          df[, .(count = sum(count)), by = .(sex, age_group)]

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

      }
    )

    df[, crude_estimate := count * life_expectancy]

    if (substance %in% c("both", "alcohol")) {

      midpoints <- midpoint_ages()

      df <-
        data.table::merge.data.table(df,
                                     midpoints,
                                     by = "age_group")

      df[, adjusted_estimate :=
          compute_yll_adjusted(number_deaths = count,
                                average_age_death = midpoint,
                                model_life_expectancy = life_expectancy)]

      df <- df[, lapply(.SD, sum),
               by = .(substance, age_group),
               .SDcols = c("count", "crude_estimate", "adjusted_estimate")]

      df <- df[, .(age_group,
                   substance,
                   count,
                   crude_estimate,
                   adjusted_estimate)]

    } else {

      df[, adjusted_estimate :=
          compute_yll_adjusted(number_deaths = count,
                                average_age_death = age,
                                model_life_expectancy = life_expectancy)]

      df <- df[, lapply(.SD, sum),
               by = .(age),
               .SDcols = c("count", "crude_estimate", "adjusted_estimate")]
    }

    return(df)
  }

compute_yll_adjusted <-
  function(number_deaths,
           average_age_death,
           model_life_expectancy,
           discount_rate = 0.03,
           beta_constant = 0.04,
           modulation_constant = 0,
           adjustment_constant = 0.1658) {
    ## abbreviate inputs
    N <- number_deaths
    a <- average_age_death
    L <- model_life_expectancy
    r <- discount_rate
    b <- beta_constant
    K <- modulation_constant
    CC <- adjustment_constant
    ## do calculations
    if (discount_rate == 0) {
      N * (K * CC * ((exp(-b * a)) / b^2) *
          ((exp(-b * L)) * (-b * (L + a) - 1) - (-b * a - 1)) +
          ((1 - K) * L))
    } else {
      N * (K * ((CC * exp(r * a)) / (-(r + b)^2)) *
             ((exp(-(r + b) * (L + a)) * (-(r + b) * (L + a) - 1)) -
                (exp(-(r + b) * a) * (-(r + b) * a - 1))) + ((1 - K) / r) *
             ((1 - exp(-r * L))))
    }
  }
