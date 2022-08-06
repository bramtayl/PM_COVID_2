library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

correlations <-
  read_csv(file.path("data", "monthly", "combined.csv")) %>%
  # rename for nicer plot labels
  rename(
    Date = date,
    `Proportion Black residents` = proportion_black,
    `Proportion Hispanic residents` = proportion_hispanic,
    `PM2.5 concentration` = PM2.5_concentration
  ) %>%
  mutate(
    # transformations for graph
    monthly_mortality = monthly_covid_deaths / population,
    `Log(population density)` = log(population_density),
    `Log(median household income)` = log(median_household_income)
  ) %>%
  select(
    Date,
    monthly_mortality,
    `Proportion Hispanic residents`,
    `Proportion Black residents`,
    `Log(median household income)`,
    `PM2.5 concentration`,
    `Log(population density)`
  ) %>%
  # to long form
  pivot_longer(
    c(
      `Proportion Hispanic residents`,
      `Proportion Black residents`,
      `Log(median household income)`,
      `PM2.5 concentration`,
      `Log(population density)`
    ),
    names_to = "Correlate of lagged 30-day COVID-19 mortality rate",
    values_to = "variable_value"
  ) %>%
  # remove missing data for `cor`
  filter(!is.na(monthly_mortality) & !is.na(variable_value)) %>%
  # calculate correlations for each variable and date
  group_by(Date, `Correlate of lagged 30-day COVID-19 mortality rate`) %>%
  summarize(
      correlation = cor(
          variable_value, 
          monthly_mortality
      )
  ) %>%
  ungroup()

# NOTE: x_axis_dates is defined in plot_coefficients.r
plot = x_axis_dates(
  ggplot(
      correlations %>%
      # just the first year of the pandemic
      filter(Date <= as.Date("2021-04-30"))
  ) +
  aes(
    x = Date,
    y = correlation,
    color = `Correlate of lagged 30-day COVID-19 mortality rate`
  ) +
  geom_line() +
  # get rid of gray background
  theme_bw() +
  labs(y = "r (correlation coefficient)")
)

ggsave(file.path("results", "monthly", "color_graph.png"),
  plot = plot,
  device = "png",
  # more wide than tall
  width = 10,
  height = 4,
  units = "in"
)