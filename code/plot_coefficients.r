library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

read_coefficients = function(analysis_date, folder) {
  read_csv(file.path(
    "data",
    folder,
    "coefficients",
    paste0(analysis_date, ".csv")
  ))
}

combine_coefficients = function(folder,
  # for the 95% CI
  confidence_interval_z_score = 1.96
) {
  # all dates with coefficients
  dates_data = 
    tibble(
        date = ymd(dir(file.path("data", folder, "coefficients")))
    ) %>%
    arrange(date)

  dates_data %>%
  group_by(date) %>%
  summarize(read_coefficients(date, folder)) %>%
  ungroup() %>%
  # these estimates are logged because of the negative binomial regression
  rename(log_estimate = estimate) %>%
  mutate(
    lower_bound = exp(log_estimate - confidence_interval_z_score * standard_error),
    estimate = exp(log_estimate),
    upper_bound = exp(log_estimate + confidence_interval_z_score * standard_error),
    Period = ifelse(
      between(date, as.Date("2020-04-18"), as.Date("2020-09-07")),
      "Considered by Wu et al.",
      "Not considered by Wu et al."
    )
  )
}

x_axis_dates = function(plot) {
  plot +
  scale_x_date(
    date_breaks = "1 month",
    # three letter month abbreviation, 4 digit years
    date_labels = "%b %Y",
    # don't expand the axis
    expand = c(0, 0)
  ) +
  # diagonal so labels will fit
  theme(
    axis.text.x = element_text(
      angle = 60,
      # right justify
      hjust = 1
    )
  )
}

MMR_plot = function(plot) {
  x_axis_dates(
    plot +
    geom_line() +
    # transparent confidence interval band
    geom_ribbon(alpha = 0.3) +
    # get rid of gray background
    theme_bw() +
    labs(
      y = "Mortality Rate Ratios",
      x = ""
    ) +
    # dashed line at y = 1
    geom_hline(yintercept = 1, linetype = "dashed")
  )
}

save_plot = function (data, folder) {
  time_series_plot = MMR_plot(
    ggplot(data) +
    aes(
      x = date,
      y = estimate,
      ymin = lower_bound,
      ymax = upper_bound,
      fill = Period
    )
  )

  file = 
    first(data$variable) %>%
    # replace all symbols with underscores for valid file names
    gsub("[^a-zA-Z0-9]", "_", .) %>%
    paste0(".png") %>%
    file.path("results", folder, .)

  ggsave(
    file,
    plot = time_series_plot,
    device = "png",
    # more wide than tall
    width = 8,
    height = 3,
    units = "in"
  )

  NULL
}

master_coefficients = combine_coefficients("master")

master_coefficients %>%
  group_by(variable) %>%
  summarize(save_plot(cur_data_all(), "master"))

# Wu et al.'s figure
early_plot = MMR_plot(
  ggplot(
    master_coefficients %>%
    filter(variable == "PM2.5_concentration" & Period == "Considered by Wu et al.")
  ) +
  aes(
    x = date,
    y = estimate,
    ymin = lower_bound,
    ymax = upper_bound
  )
)

ggsave(
  file.path("results", "master", "early_plot.png"),
  plot = early_plot,
  device = "png",
  # more wide than tall
  width = 8,
  height = 3,
  units = "in"
)

updated_PM_coefficients = 
  combine_coefficients("updated_data") %>%
  filter(variable == "PM2.5_concentration")

# write out the final estimates for the updated data
updated_PM_coefficients %>%
  arrange(date) %>%
  filter(date == last(date)) %>%
  write_csv(file.path("data", "updated_data", "final_estimates.csv"))

save_plot(updated_PM_coefficients, "updated_data")

combine_coefficients("monthly") %>%
  filter(variable == "PM2.5_concentration") %>%
  save_plot("monthly")
