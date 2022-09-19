source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "25-digital-society-project"

dat_raw <-
  fread("./data/2020 sfr model data/DigitalSocietyProject-v4-CSV/DSP-Dataset-v4.csv")
dat_clean <- dat_raw %>%
  dplyr::select(iso3c = country_text_id,
         year,
         value = v2smarrest) %>%
  dplyr::filter(iso3c %!in% c("ZZB")) %>% #remove Zanzibar, that's fine
  #adjust iso3c for a couple of countries
  dplyr::mutate(iso3c = ifelse(iso3c == "SML",
                        "SOM",
                        ifelse(iso3c == "PSG",
                               "PSE",
                               iso3c))) %>%
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(value = min(value)) %>%
  dplyr::mutate(variablename = "Arrests from online content",
         iso3c_CONVERT = country.code.name(iso3c)) %>%
  dplyr::ungroup()

# Adding the checks
check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_clean,"iso3c", "year", SCRIPT)
check_duplicates(dat_clean,"iso3c", "year", "variablename",SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

check_nas(dat_clean, SCRIPT) # drop the iso3c convert before running this
raw.data$dsp <- dat_clean
rmExcept("raw.data")