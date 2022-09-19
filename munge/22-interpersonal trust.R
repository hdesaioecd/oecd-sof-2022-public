source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "22-interpersonal trust"

# https://www.prosperity.com/about/resources
dat_raw_path <-
  "./data/2020 sfr model data/2021_Full_Data_Set_-_Legatum_Prosperity_Index.xlsx"
dat_raw <- read_excel(dat_raw_path, "Indicators x 300") %>%
  dplyr::rename(iso3c = area_name) %>%
  dplyr::filter(indicator_name == "Generalised interpersonal trust") %>%
  dplyr::select(iso3c, contains("score"))
dat_raw$iso3c <- country.code.name(dat_raw$iso3c)
dat_clean <- dat_raw %>%
  tidyr::pivot_longer(names_to = "year",
               values_to = "value", -iso3c) %>%
  dplyr::mutate(year = gsub("score_", "", year),
         variablename = "Interpersonal trust")

dat_clean$year <- as.integer(dat_clean$year)

dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_clean,"iso3c", "year", SCRIPT)
check_duplicates(dat_clean,"iso3c", "year", "variablename",SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

check_nas(dat_clean, SCRIPT) # drop the iso3c convert before running this

raw.data$trust <- dat_clean
rmExcept("raw.data")