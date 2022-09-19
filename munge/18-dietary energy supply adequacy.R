source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")


SCRIPT = "18-dietary-energy-supply"

# http://data.un.org/Data.aspx?q=energy&d=FAO&f=itemCode%3A21010
dat_raw <-
  fread("./data/2020 sfr model data/FAOSTAT_data_3-31-2022.csv")
dat_clean <- dat_raw %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    variablename = "Average dietary energy supply adequacy (percent) (3-year average)",
    iso3c = country.code.name(area),
    year = sub("(^[^-]+)-.*", "\\1", year),
    year = as.numeric(year),
    year = year + 1 #adjustment based on Kazu's advice (in light of source data)
  ) %>%
  dplyr::select(iso3c, area, value, year, variablename) %>%
  dplyr::filter(
    area %!in% c(
      "China",
      "East Asia (excluding China)",
      "South Asia (excluding India)",
      "North Africa (excluding Sudan)",
      "Micronesia"
    )
  ) %>%
  drop_na()

# NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT) # this is different from other scripts
check_coverage(dat_clean, "iso3c", "year", SCRIPT)
check_duplicates(dat_clean, "iso3c", "year", "variablename", SCRIPT)
check_nas(dat_clean, SCRIPT) # drop the iso3c convert before running this

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c, year, value, variablename)
raw.data$food_supply_adequacy <- dat_clean
rmExcept("raw.data")