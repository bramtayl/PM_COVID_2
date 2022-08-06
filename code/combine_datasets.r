library(dplyr)
library(glmmTMB)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(tigris)

# Reuse tigris data
options(tigris_use_cache = TRUE)

reference_date = as.Date("2020-03-30")

state_data <-
    states() %>%
    as_tibble() %>%
    select(
        state_code = GEOID,
        state_name = NAME
    )

# JHU reports aggregates for NYC on some dates
# even though all the boroughs are separate counties.
# To aggregate NYC, make a new unit of observation called a "metro".
# A metro is usually the same as the county, but for NYC, the metro includes
# all the boroughs
NYC_counties <-
    tibble(
        state_name = "New York",
        county_name = c("New York", "Bronx", "Kings", "Queens", "Richmond"),
        # Manhattan is the primary county that subsumes the others
        primary = c(TRUE, FALSE, FALSE, FALSE, FALSE),
        # The same as the fips code for manhattan
        metro_code = 36061
    )

county_data <-
    # Use an earlier year because the PM data references counties that don't
    # exist anymore
    counties(year = 2014) %>%
    as_tibble() %>%
    select(
        county_code = GEOID,
        county_name = NAME,
        state_code = STATEFP
    ) %>%
    mutate(county_code = as.numeric(county_code)) %>%
    # Replace the state codes with names
    left_join(state_data) %>%
    select(-state_code) %>%
    # Add in NYC data
    left_join(NYC_counties) %>%
    mutate(
        # All counties are primary except in NYC
        primary = coalesce(primary, TRUE),
        # The metro code equals the county code everywhere but NYC
        metro_code = coalesce(metro_code, county_code),
    )

# Wu et al. read mortality data for 4 age groups from CDC Wonder.
# Wu et al. did not use the mortality data, but only the population counts, to calculate demographic proportions
age_group_files <- tibble(
    file = file.path("data", c(
        "county_old_mortality.txt",
        "county_014_mortality.txt",
        "county_1544_mortality.txt",
        "county_4564_mortality.txt"
    )),
    variable = c(
        "proportion_65_and_over",
        "proportion_14_and_under",
        "proportion_15_to_44",
        "proportion_45_to_64"
    )
)

age_group_data <-
    age_group_files %>%
    group_by(variable) %>%
    summarize(
        # Use read.table instead of read_delim, because it handles
        # misformatted files better
        read.table(file, sep = "", header = TRUE) %>%
            select(
                county_code = County.Code,
                age_group_population = Population
            )
    )

DEMOGRAPHIC_DATA <-
    read.table(file.path(
        "data",
        "county_base_mortality.txt"
    ), sep = "", header = TRUE) %>%
    select(
        county_code = County.Code,
        whole_population = Population
    ) %>%
    # find all combinations of age groups and county codes
    left_join(
        age_group_files %>%
        select(variable),
        by = character()
    ) %>%
    left_join(age_group_data) %>%
    mutate(
        # Fill 0 for missing
        proportion = coalesce(age_group_population / whole_population, 0),
    ) %>%
    select(variable, county_code, proportion) %>%
    pivot_wider(
        names_from = variable,
        values_from = proportion
    )

get_covid_data <- function(a_date) {
    read_csv(file.path(
        "data",
        "covid",
        paste0(format(a_date, format = "%m-%d-%Y"), ".csv")
    )) %>%
        select(
            county_code = FIPS,
            county_name = Admin2,
            cumulative_covid_deaths = Deaths,
            confirmed_covid_cases = Confirmed,
            country_abbreviation = Country_Region
        ) %>%
        # only known US counties
        filter(country_abbreviation == "US" & !is.na(county_code)) %>%
        select(-country_abbreviation)
}

# all dates with covid data available
DATES_DATA = 
    tibble(
        date = mdy(dir(file.path("data", "covid")))
    ) %>%
    arrange(date)

daily_covid_data <-
    DATES_DATA %>%
    group_by(date) %>%
    summarize(get_covid_data(date)) %>%
    ungroup() %>%
    mutate(
        # Keep track of whether covid data exists for counties.
        in_contemporary_covid_data = TRUE
    )

# JHU uses "New York City" to mean the aggregated 5 boroughs
# Figure out which dates JHU aggregated the boroughs on
covid_aggregation_data <-
    daily_covid_data %>%
    group_by(date) %>%
    summarize(
        aggregated_by_metro = "New York City" %in% county_name
    )

# Just data from when JHU aggregated NYC
already_aggregated <-
    daily_covid_data %>%
    select(-county_name) %>%
    inner_join(
        covid_aggregation_data %>%
            filter(aggregated_by_metro)
    ) %>%
    select(-aggregated_by_metro) %>%
    rename(metro_code = county_code)

metro_covid_data <-
    # Aggregate the NYC data ourselves after JHU stopped aggregating it.
    daily_covid_data %>%
    select(-county_name) %>%
    inner_join(
        covid_aggregation_data %>%
            filter(!aggregated_by_metro)
    ) %>%
    select(-aggregated_by_metro) %>%
    # add in metro codes
    left_join(
        county_data %>%
            select(metro_code, county_code)
    ) %>%
    group_by(date, metro_code) %>%
    summarize(
        # Sum these 2 statistics
        cumulative_covid_deaths = sum(cumulative_covid_deaths),
        confirmed_covid_cases = sum(confirmed_covid_cases),
        # TRUE if any borough was mentioned in the original covid data
        in_contemporary_covid_data = any(in_contemporary_covid_data)
    ) %>%
    ungroup() %>%
    bind_rows(already_aggregated)

# Wu et al. found counties with no reported covid on an early date.
# JHU might have omitted these counties on a later date.
no_early_covid <-
    metro_covid_data %>%
    filter(
        date == reference_date &
            confirmed_covid_cases == 0 &
            cumulative_covid_deaths == 0
    ) %>%
    select(metro_code) %>%
    mutate(in_no_early_covid = TRUE)

# Find date of first case in each county
FIRST_COVID_DATA <-
    metro_covid_data %>%
    filter(confirmed_covid_cases > 0) %>%
    group_by(metro_code) %>%
    # for each metro code, get the row with the first date
    arrange(date) %>%
    slice(1) %>%
    ungroup() %>%
    select(
        metro_code,
        first_confirmed_date = date
    )

WU_COVID_DATA <-
    full_join(
        metro_covid_data,
        # Replicate `no_early_covid` for all dates
        # Otherwise, some dates will be missing after the full join
        full_join(
            no_early_covid,
            DATES_DATA,
            by = character()
        )
    ) %>%
    mutate(
        cumulative_covid_deaths =
        # Fill in zero if the county was mentioned early but not later
            ifelse(
                is.na(in_contemporary_covid_data) & in_no_early_covid,
                0,
                cumulative_covid_deaths
            )
    ) %>%
    filter(
        # Wu only used counties mentioned in the covid data or in the early covid data
        in_contemporary_covid_data | in_no_early_covid
    )

HEALTH_DATA <-
    read_csv(
        file.path(
            "data",
            "analytic_data2020.csv"
        ),
        col_types = list(
            fipscode = col_double()
        ),
        # The first row just has descriptions.
        # Might be nice to read them in and use them for selection instead.
        skip = 1,
        # Parse the whole thing first to verify column types
        guess_max = 3194
    ) %>%
    select(
        county_code = fipscode,
        proportion_obsese = v011_rawvalue,
        proportion_smoke = v009_rawvalue
    )

HOSPITALS_DATA <-
    read_csv(
        file.path(
            "data",
            "hospitals.csv"
        ),
        # they also use NOT AVAILABLE for misisng data
        na = c("NA", "NOT AVAILABLE"),
        col_types = list(
            COUNTYFIPS = col_double()
        ),
    ) %>%
    select(
        county_code = COUNTYFIPS,
        hospital_beds = BEDS
    ) %>%
    mutate(
        # Negative numebrs are placeholders for missing in this variable.
        hospital_beds = ifelse(hospital_beds < 0, NA, hospital_beds)
    ) %>%
    group_by(county_code) %>%
    summarise(
        # Ignore missing values.
        hospital_beds = sum(hospital_beds, na.rm = TRUE)
    )

POLICY_DATA <-
    read_csv(file.path(
        "data",
        "state_policy0410.csv"
        # Read in the whole thing before predicting column types
    ), n_max = 51) %>%
    rename(social_distancing_date = `Stay at home/ shelter in place`) %>%
    select(
        state_name = State,
        social_distancing_date
    ) %>%
    mutate(
        # "0" means missing in this data
        social_distancing_date =
            mdy(ifelse(social_distancing_date == "0", NA, social_distancing_date)),
        in_policy_data = TRUE
    )

TEMPERATURE_DATA <- 
    read_csv(
        file.path(
            "data",
            "temp_seasonal_county.csv"
        ),
        col_types = list(
            fips = col_double()
        )
    ) %>%
    select(
        county_code = fips,
        winter_mean_maximum_temperature = winter_tmmx,
        summer_mean_maximum_temperature = summer_tmmx,
        winter_mean_relative_humidity = winter_rmax,
        summer_mean_relative_humidity = summer_rmax
    ) %>%
    group_by(county_code) %>%
    summarise(
        winter_mean_maximum_temperature = mean(winter_mean_maximum_temperature),
        summer_mean_maximum_temperature = mean(summer_mean_maximum_temperature),
        winter_mean_relative_humidity = mean(winter_mean_relative_humidity),
        summer_mean_relative_humidity = mean(summer_mean_relative_humidity)
    )

# For monthly covid mortality, we need to make sure we have a value for every possible date/metro combination
# even if it's missing
# to make it easier, first get rid of duplicated counties in the covid data
covid_data_unique <-
    metro_covid_data %>%
    # remove duplications by just using the first row
    group_by(date, metro_code) %>%
    slice(1) %>%
    ungroup()

MONTHLY_COVID_DATA <-
    covid_data_unique %>%
    select(metro_code) %>%
    unique %>%
    full_join(
        DATES_DATA,
        # find all possible date/metro combinations
        by = character()
    ) %>%
    left_join(covid_data_unique) %>%
    group_by(metro_code) %>%
    arrange(date) %>%
    mutate(
        # cumulative covid deaths must start at zero and always increase, by definition
        # we can use fact this to fill in a lot of missing data
        cumulative_covid_deaths = 
            cummax(coalesce(cumulative_covid_deaths, 0)),
        # find the the total within the last 30 days
        monthly_covid_deaths =
            cumulative_covid_deaths -
            lag(cumulative_covid_deaths, 30)
    ) %>%
    ungroup()

# combine the controls (really everything except the covid data)
# do these separately because we will have to aggregate NYC first
# make a function so we can do this for both branches
combine_controls <- function(
    census_branch = "master",
    pm_branch = "master",
    dates_data = DATES_DATA,
    first_covid_data = FIRST_COVID_DATA,
    temperature_data = TEMPERATURE_DATA,
    health_data = HEALTH_DATA,
    demographic_data = DEMOGRAPHIC_DATA,
    hospitals_data = HOSPITALS_DATA,
    policy_data = POLICY_DATA,
    Wu_run_date = as.Date("2020-09-07")
) {
    # the census data is different between the two branches
    # we are not sure why
    raw_census_data <- read_csv(
        file.path("data", census_branch, "census_county_interpolated.csv"),
        col_types = list(
            fips = col_double()
        )
    )

    census_data <-
        # The two branches have slighlty different variable names
        (if (census_branch == "master") {
            raw_census_data %>%
                select(
                    county_code = fips,
                    year,
                    population,
                    proportion_poor = poverty,
                    population_density = popdensity,
                    median_house_value = medianhousevalue,
                    proportion_black = pct_blk,
                    median_household_income = medhouseholdincome,
                    proportion_owner_occupied = pct_owner_occ,
                    proportion_hispanic = hispanic,
                    proportion_no_high_school = education
                )
        } else {
            raw_census_data %>%
                select(
                    county_code = fips,
                    year,
                    population,
                    proportion_poor = poverty,
                    population_density,
                    median_house_value,
                    proportion_black = blk_pct,
                    median_household_income,
                    proportion_owner_occupied = owner_occupied,
                    proportion_hispanic = hispanic_pct,
                    proportion_no_high_school = no_grad
                )
        }) %>%
        # on the updated_data branch, this is the 2012-2016 5-year ACS data
        # there is no code for calculating this in the master branch though
        filter(year == 2016) %>%
        mutate(
            population_density_quantile = cut(
                population_density,
                quantile(
                    population_density,
                    probs = seq(0, 1, 0.2),
                    # there's some missing data on the updated_data branch
                    na.rm = TRUE
                ),
                # so the lowest doesn't get left out
                include.lowest = TRUE
            ),
            # keep track of whether census data exists for counties
            in_census_data = TRUE
        )

    # the PM data is different between the two branches
    # the new estimates include two extra years
    pm_data =
        read_csv(file.path("data", pm_branch, "county_pm25.csv")) %>%
            select(
                county_code = fips,
                PM2.5_concentration = pm25
            ) %>%
            group_by(county_code) %>%
            summarise(
                PM2.5_concentration = mean(PM2.5_concentration)
            ) %>%
            # Keep track of whether PM data exists for counties.
            mutate(in_pm_data = TRUE)

    county_controls <-
        county_data %>%
        left_join(pm_data) %>%
        left_join(temperature_data) %>%
        left_join(census_data) %>%
        left_join(health_data) %>%
        left_join(demographic_data) %>%
        left_join(hospitals_data) %>%
        mutate(
            # Fill 0 for missing
            hospital_beds = coalesce(hospital_beds, 0)
        ) %>%
        # Only use counties mentioned in both the PM data and the census data
        # The pm data only includes counties in the continental US
        filter(in_pm_data & in_census_data) %>%
        group_by(metro_code) %>%
        mutate(
            # There is a clear bug in Wu et al's code where they accidentally aggregate population too early
            buggy_population =
                ifelse(
                    primary,
                    sum(population),
                    population
                )
        ) %>%
        ungroup()

    metro_controls = 
        county_controls %>%
        # we don't need to group by state, because no metro overlaps two states
        # but we need the state names later to join in the policy data
        group_by(state_name, metro_code) %>%
        summarize(
            # weight most things by buggy population
            PM2.5_concentration = 
                weighted.mean(PM2.5_concentration, buggy_population),
            proportion_poor =
                weighted.mean(proportion_poor, buggy_population),
            median_house_value =
                weighted.mean(median_house_value, buggy_population),
            median_household_income =
                weighted.mean(median_household_income, buggy_population),
            proportion_owner_occupied =
                weighted.mean(proportion_owner_occupied, buggy_population),
            proportion_no_high_school =
                weighted.mean(proportion_no_high_school, buggy_population),
            proportion_black =
                weighted.mean(proportion_black, buggy_population),
            proportion_hispanic =
                weighted.mean(proportion_hispanic, buggy_population),
            proportion_65_and_over =
                weighted.mean(proportion_65_and_over, buggy_population),
            proportion_15_to_44 =
                weighted.mean(proportion_15_to_44, buggy_population),
            proportion_45_to_64 =
                weighted.mean(proportion_45_to_64, buggy_population),
            proportion_obsese =
                weighted.mean(proportion_obsese, buggy_population),
            proportion_smoke =
                weighted.mean(proportion_smoke, buggy_population),
            summer_mean_maximum_temperature =
                weighted.mean(summer_mean_maximum_temperature, buggy_population),
            summer_mean_relative_humidity =
                weighted.mean(summer_mean_relative_humidity, buggy_population),
            winter_mean_maximum_temperature =
                weighted.mean(winter_mean_maximum_temperature, buggy_population),
            winter_mean_relative_humidity =
                weighted.mean(winter_mean_relative_humidity, buggy_population),
            population_density =
                weighted.mean(population_density, population),
            # Sum these last 2
            hospital_beds = sum(hospital_beds),
            population = sum(population)
        ) %>%
        left_join(
            county_controls %>%
                # Wu et al. used the quantile of the primary county to represent the whole metro.
                # This probably doesn't make a difference though, because all the NYC boroughs are likely in the highest quantile
                filter(primary) %>%
                select(
                    metro_code,
                    population_density_quantile
                )
        ) %>%
        left_join(first_covid_data) %>%
        left_join(policy_data)
    
    full_join(
        # Replicate the control data for all dates.
        metro_controls,
        dates_data,
        by = character()
    ) %>%
    mutate(
        # If the first covid data is after the date of analysis, set to `days_since_covid` to 0.
        # also fill in 0 for missing
        days_since_covid =
            ifelse(
                first_confirmed_date <= date,
                date - first_confirmed_date + 1,
                NA
            ) %>%
                coalesce(0),
        # If the first social distancing mandate is after Wu's run date, set `days_since_social_distancing` to 0
        # also fill in 0 for missing
        days_since_social_distancing =
            as.numeric(Wu_run_date - social_distancing_date) %>%
                coalesce(0)
    )
}

master_controls = 
    combine_controls(
        census_branch = "master",
        pm_branch = "master"
    )

# save for the master branch
master_controls %>%
    inner_join(WU_COVID_DATA) %>%
    write_csv(
        file.path(
            "data", "master", "combined.csv"
        )
    )

# since we've cleaned the monthly covid data, we need to keep it seperate
master_controls %>%
    inner_join(MONTHLY_COVID_DATA) %>%
    write_csv(
        file.path(
            "data", "monthly", "combined.csv"
        )
    )

# also save for the updated_data branch
combine_controls(
    census_branch = "updated_data",
    pm_branch = "updated_data"
) %>%
    inner_join(WU_COVID_DATA) %>%
    write_csv(
        file.path(
            "data", "updated_data", "combined.csv"
        )
    )