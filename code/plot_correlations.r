library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# remove duplicate metros
all_data_unique <-
    read_csv(file.path("data", "master", "combined.csv")) %>%
    # rename for nicer plot labels
    rename(Date = date) %>%
    group_by(Date, metro_code) %>%
    slice(1) %>%
    ungroup()

# find all dates
dates_data = 
    all_data_unique %>%
    select(Date) %>%
    unique()

all_dates_and_counties <-
    # make a row for all dates and counties, with some missing mortality data
    # recreate the controls dataset
    all_data_unique %>%
    # rename for nicer plot labels
    rename(
        `Proportion Black residents` = proportion_black,
        `Proportion Hispanic residents` = proportion_hispanic,
        `PM2.5 concentration` = PM2.5_concentration
    ) %>%
    group_by(metro_code) %>%
    summarize(
        population_density = first(population_density),
        `Proportion Black residents` = first(`Proportion Black residents`),
        `Proportion Hispanic residents` = first(`Proportion Hispanic residents`),
        median_household_income = first(median_household_income),
        `PM2.5 concentration` = first(`PM2.5 concentration`)
    ) %>%
    ungroup() %>%
    # replicate controls for all dates
    full_join(
        dates_data,
        by = character()
    ) %>%
    # join back in the covid data
    left_join(
        all_data_unique %>%
            select(metro_code, Date, cumulative_covid_deaths)
    ) %>%
    group_by(metro_code) %>%
    arrange(Date) %>%
    mutate(
        # covid deaths can't decrease
        # and started at 0
        # so we can use cummax and coalesce to fill in covid data on
        # days when it is missing
        cumulative_covid_deaths_filled =
            cummax(coalesce(cumulative_covid_deaths, 0)),
        # find the proportion of the total within the last 30 days
        proportion_so_far = 
            (
                cumulative_covid_deaths_filled -
                lag(cumulative_covid_deaths_filled, 30)
            # the last is the total
            ) / last(cumulative_covid_deaths_filled)
    ) %>%
    ungroup() %>%
    # remove missing data (a small intervals at start where window goes
    # outside of the available dates)
    filter(!is.na(proportion_so_far)) %>%
    # transformations for graph
    mutate(
        `Log(population density)` = log(population_density),
        `Log(median household income)` = log(median_household_income)
    )

correlations <-
    all_dates_and_counties %>%
    select(
        Date,
        proportion_so_far,
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
        names_to = "Variable",
        values_to = "variable_value"
    ) %>%
    # calculate correlations for each variable and date
    group_by(Date, Variable) %>%
    summarize(
        correlation = cor(
            variable_value, 
            proportion_so_far
        )
    ) %>%
    ungroup()

plot = 
  ggplot(
      correlations %>%
      filter(Date <= as.Date("2021-04-30"))) +
  aes(
    x = Date,
    y = correlation,
    color = Variable
  ) +
  geom_line() +
  theme_bw() +
  scale_x_date(
    date_breaks = "1 month",
    # Three letter month abbreviation, 4 digit years
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  # diagonal so labels will fit
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1
    )
  ) +
  labs(y = "Correlation with the proportion of\nCOVID-19 deaths that occured within\nthe last 30 days")

ggsave(file.path("results", "master", "color_graph.png"),
  plot = plot,
  device = "png",
  width = 10,
  height = 4,
  units = "in"
)