source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "05-debt-to-GDP-ratio"

filename <- "./data/2020 sfr model data/WEOApr2022all.xlsx"
dat_raw <- read_excel(filename) %>% janitor::clean_names()
setDT(dat_raw)
drop_cols <- c(
  "weo_country_code",
  "subject_descriptor",
  "subject_notes",
  "units",
  "scale",
  "country_series_specific_notes",
  "weo_country_code",
  "weo_subject_code"
)
dat_clean <-
  dat_raw[weo_subject_code == "GGXWDG_NGDP",!..drop_cols]

#need to follow the following steps to avoid using IMF estimates, especially for later years
#where they may lead to implausible results (such as in the case of Venezuela, where estimates are highly uncertain)
#check to ensure that you are not missing any data that the estimates will capture
#but otherwise, use most recent year ("estimates start after")

dat_long <-
  dat_clean %>%
  dplyr::mutate_all(~ as.character(.)) %>%
  dplyr::select(-iso) %>%
  tidyr::pivot_longer(names_to = "year",
                      values_to = "value",
                      -c("country",
                         "estimates_start_after")) %>%
  dplyr::mutate(
    year = gsub("x", "", year),
    estimates_start_after = as.integer(estimates_start_after),
    value = gsub("n/a", NA, value),
    value = as.numeric(value),
    country = gsub("S„o TomÈ and PrÌncipe", "Sao Tome and Principe", country),
    country = gsub("CÙte d'Ivoire", "Cote d'Ivoire", country) #fix the issue with STP and CIV
  ) %>%
  tidyr::drop_na(value)

#NEW
dat_long$iso3c_CONVERT <- country.code.name(dat_long$country)

# Check the country conversion
check_convert_iso(dat_long, country, iso3c_CONVERT, SCRIPT)

dat_long$iso3c <- dat_long$iso3c_CONVERT

#follow these steps carefully:
dat_subset <-
  dat_long %>%
  dplyr::filter(estimates_start_after == year) %>%
  dplyr::select(-estimates_start_after)

dat_subset_early_yrs <-
  dat_long %>%
  dplyr::filter(year < estimates_start_after) %>%
  dplyr::select(-estimates_start_after)

#bind the above two datasets - this means that estimates will not be used -
# additional checks show that this is fine practice and works
dat_final <-
  rbind(dat_subset, dat_subset_early_yrs) %>%
  dplyr::arrange(year) %>%
  dplyr::filter(year != 2022)

dat_final <- na.omit(dat_final)
dat_final$variablename <- "General government gross debt"
dat_final$iso3c <- country.code.name(dat_final$iso3c)

# Perform additional checks here
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final, "iso3c", "year", SCRIPT)
check_duplicates(dat_final, "iso3c", "year", NULL, SCRIPT)
check_nas(dat_final, SCRIPT)

dat_final = dat_final %>% select(iso3c, year, value, variablename)
raw.data$gdpdebt <- dat_final
rmExcept("raw.data")