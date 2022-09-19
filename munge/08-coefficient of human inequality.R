source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "08-coefficient of human inequality"

dat_path <-
  "./data/2020 sfr model data/IHDI_HDR2020_040722.csv"

dat_raw <- fread(dat_path, header = T) %>%
  select(country, contains("coef_ineq")) %>%
  pivot_longer(names_to = "year",
               values_to = "value",-country) %>%
  mutate(
    year = gsub("coef_ineq_", "", year),
    year = as.integer(year),
    variablename = "Coefficient of human inequality",
    iso3c_CONVERT = country.code.name(country)
  )
dat_raw$iso3c <- country.code.name(dat_raw$country)

check_convert_iso(dat_raw, "country", "iso3c_CONVERT", SCRIPT)
check_nas(dat_raw)
dat_clean <- na.omit(dat_raw)
check_coverage(dat_clean %>% mutate(iso3c = country.code.name(iso3c)), "iso3c", "year", SCRIPT)

dat_final = dat_clean %>% 
  select(iso3c = country, year, value, variablename)
raw.data$coef_humineq <- dat_final
rmExcept("raw.data")