library(dplyr)
library(glmmTMB)
library(readr)

run_regression = function(one_day_data) {
  glmmTMB(
    cumulative_covid_deaths ~ 
      PM2.5_concentration + 
      factor(population_density_quantile) +
      scale(proportion_poor) +
      scale(log(median_house_value)) +
      scale(log(median_household_income)) +
      scale(proportion_owner_occupied) +
      scale(proportion_no_high_school) +
      scale(proportion_black) +
      scale(proportion_hispanic) +
      scale(proportion_65_and_over) +
      scale(proportion_15_to_44) +
      scale(proportion_45_to_64) +
      scale(days_since_social_distancing) +
      scale(days_since_covid) + 
      scale(hospital_beds/population) +
      scale(proportion_obsese) +
      scale(proportion_smoke) +
      scale(summer_mean_maximum_temperature) +
      scale(winter_mean_maximum_temperature) +
      scale(summer_mean_relative_humidity) +
      scale(winter_mean_relative_humidity) +
      offset(log(population)) +
      (1 | state_name),
    data = one_day_data,
    family = nbinom2,
    ziformula = ~ 1
  )
}

save_coefficients = function(
  one_day_data,
  branch = "master"
) {
  # print date to show progress
  analysis_date = first(one_day_data$date)
  print(analysis_date)

  run_regression(one_day_data) %>%
    # pull out coefficients
    summary %>%
    .$coefficients %>%
    .$cond %>%
    # format as table and rename
    as_tibble(rownames = "variable") %>%
    rename(
      estimate = Estimate,
      standard_error = `Std. Error`,
      z_value = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    # write to file
    write_csv(file.path(
      "data",
      branch,
      "coefficients",
      paste0(analysis_date, ".csv")
    ))
  
  NULL
}

# if you stop the code before R finished running it
# you can resume by choosing a later first_date
Wu_analysis = function (data, branch, first_date = as.Date("2020-04-18")) {
    data %>%
    filter(date >= first_date) %>%
    group_by(date) %>%
    summarize(save_coefficients(cur_data_all(), branch = branch))
}

# do for two separate branches
master_data = read_csv(file.path("data", "master", "combined.csv"))
Wu_analysis(master_data, "master")

updated_data = read_csv(file.path("data", "updated_data", "combined.csv"))
Wu_analysis(updated_data, "updated_data")
