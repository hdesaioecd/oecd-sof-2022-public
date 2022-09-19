source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

#using the GOCI: https://globalinitiative.net/analysis/ocindex-2021/

SCRIPT = "14-criminal networks"

file_path <-
  "./data/2020 sfr model data/global_oc_index_2021.xlsx"
dat_raw <- read_excel(file_path, "Full_dataset")
dat_clean <- setDT(dat_raw)[, .(
  iso3c = country.code.name(`Country`),
  value = `Criminal networks`,
  year = 2021,
  variablename = "Criminal networks"
)]

#NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT) # this is different from other scripts
check_coverage(dat_clean, "iso3c", "year", SCRIPT)
check_duplicates(dat_clean, "iso3c", "year", "variablename", SCRIPT)
dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c, year, value, variablename)
check_nas(dat_clean, SCRIPT) # drop the iso3c convert before running this
raw.data$crim_networks <- dat_clean
rmExcept("raw.data")