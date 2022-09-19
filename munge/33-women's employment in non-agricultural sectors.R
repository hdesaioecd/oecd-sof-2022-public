source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "33-womens-employment-in-non-agriculture"

dat_raw <-
  fread(
    "./data/2020 sfr model data/Share of employment in nonagriculture, female (% of total employment in nonagriculture).csv",
    header = T
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  dplyr::select(-hdi_rank)
dat_clean <- dat_raw %>%
  tidyr::pivot_longer(names_to = "year",
                      values_to = "value", -country) %>%
  dplyr::mutate(
    variablename = "Women's employment in non-agricultural sectors",
    iso3c = country.code.name(country),
    year = gsub("x", "", year),
    year = as.integer(year),
    value = as.numeric(value)
  ) %>%
  dplyr::select(-country)

# NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)
dat_final <- na.omit(dat_clean)

# Adding the checks
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final,"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", "variablename",SCRIPT)
dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c,year,value,variablename)

check_nas(dat_final, SCRIPT) 
raw.data$fem_emp <- dat_final
rmExcept("raw.data")