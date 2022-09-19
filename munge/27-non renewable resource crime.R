source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "27-non-renewable-resource-crime"

file_path <-
  "./data/2020 sfr model data/global_oc_index_2021.xlsx"
dat_raw <- read_excel(file_path, "Full_dataset") %>%
  dplyr::select(iso3c = `Country Code`,
                value = `Non-renewable resource crimes`) %>%
  dplyr::mutate(
    year = 2021,
    variablename = "Resource crimes",
    iso3c_CONVERT = country.code.name(iso3c)
  )

# Adding the checks
check_convert_iso(dat_raw, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_raw, "iso3c", "year", SCRIPT)
check_duplicates(dat_raw, "iso3c", "year", "variablename", SCRIPT)

dat_raw$iso3c = dat_raw$iso3c_CONVERT
dat_raw = dat_raw %>% select(iso3c, year, value, variablename)
check_nas(dat_raw, SCRIPT) # drop the iso3c convert before running this

raw.data$non_renewable_resource_crimes <- dat_raw
rmExcept("raw.data")