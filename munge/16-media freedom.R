source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "16-media-freedom"

# https://freedomhouse.org/sites/default/files/2022-02/All_data_FIW_2013-2022.xlsx
dat_raw <-
  read_excel(
    "./data/2020 sfr model data/All_data_FIW_2013-2022.xlsx",
    sheet = 2,
    skip = 1
  ) %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = edition - 1,
                variablename = "Free and independent media",
                year = as.integer(year)) %>%
  dplyr::select(iso3c = country_territory,
                year,
                value = d1,
                variablename) %>%
  dplyr::filter(iso3c %!in% c("Indian Kashmir",
                              "Pakistani Kashmir",
                              "Northern Cyprus"))

dat_raw$iso3c_CONVERT <- country.code.name(dat_raw$iso3c)

# TO DO
check_convert_iso(dat_raw, iso3c, iso3c_CONVERT,SCRIPT) # this is different from other scripts
check_coverage(dat_raw,"iso3c", "year", SCRIPT)
check_duplicates(dat_raw,"iso3c", "year", "variablename",SCRIPT)

dat_raw <- na.omit(dat_raw)

dat_clean <- dat_raw %>%
  group_by(year, iso3c_CONVERT, variablename) %>%
  summarise(value = min(value)) %>%
  ungroup()

check_duplicates(dat_clean,"iso3c_CONVERT", "year", "variablename",SCRIPT)

# TO DO
dat_clean$iso3c <- country.code.name(dat_clean$iso3c_CONVERT)

dat_clean = dat_clean %>% ungroup()
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

raw.data$press_freedom <- dat_clean
rmExcept("raw.data")