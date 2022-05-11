library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

first_date = as.Date("2020-04-18")
last_date = as.Date("2022-04-18")
dates_data <-
  tibble(date = seq(first_date, last_date, by = "day"))

branch = "master"

plot_for_branch = function(branch) {
  dates_data = 
    tibble(
        date = ymd(dir(file.path("data", branch, "coefficients")))
    ) %>%
    arrange(date)

  confidence_interval_z_score = 1.96

  read_coefficients = function(analysis_date, branch) {
    read_csv(file.path(
      "data",
      branch,
      "coefficients",
      paste0(analysis_date, ".csv")
    ))
  }

  all_coefficients =
    master_dates_data %>%
    group_by(date) %>%
    summarize(read_coefficients(date, branch))

  PM_coefficients = 
    all_coefficients %>%
    filter(variable == "PM2.5_concentration") %>%
    mutate(
      lower_bound = exp(estimate - confidence_interval_z_score * standard_error),
      log_estimate = exp(estimate),
      upper_bound = exp(estimate + confidence_interval_z_score * standard_error),
      Period = ifelse(
        between(date, as.Date("2020-04-18"), as.Date("2020-09-07")),
        "Considered by Wu et al.",
        "Not considered by Wu et al."
      )
    )

  # Wu et al.'s figure
  early_plot = 
    ggplot(
      PM_coefficients %>%
      filter(Period == "Considered by Wu et al.")
    ) +
    aes(
      x = date,
      y = log_estimate,
      ymin = lower_bound,
      ymax = upper_bound
    ) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    theme_bw() +
    labs(
      y = "Mortality Rate Ratios",
      x = ""
    ) +
    geom_hline(yintercept = 1, linetype = "dashed") +
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
    )

  ggsave(file.path(
    "results",
    branch,
    "early_plot.png"
  ),
    plot = early_plot,
    device = "png",
    width = 8,
    height = 3,
    units = "in"
  )

  # additional data
  additional_plot = 
    ggplot(PM_coefficients) +
    aes(
      x = date,
      y = log_estimate,
      ymin = lower_bound,
      ymax = upper_bound,
      fill = Period
    ) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    theme_bw() +
    labs(
      y = "Mortality Rate Ratios",
      x = ""
    ) +
    geom_hline(yintercept = 1, linetype = "dashed") +
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
    scale_colour_grey(aesthetics = "fill")

  ggsave(file.path(
    "results",
    branch,
    "additional_plot.png"
  ),
    plot = additional_plot,
    device = "png",
    width = 8,
    height = 3,
    units = "in"
  )
}

plot_for_branch("master")
plot_for_branch("updated_data")
