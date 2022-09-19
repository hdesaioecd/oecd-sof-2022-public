source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

# https://www.sipri.org/databases/milex

SCRIPT = "17-SIPRI-milex"

path <- "./data/2020 sfr model data/SIPRI-Milex-data-1949-2021.xlsx"
dat_raw <- read_excel(path, "Share of GDP", skip = 5)
setDT(dat_raw)
dat_raw$Notes <- NULL

dat_raw[, iso3c := country.code.name(Country)]
dat_clean <- na.omit(dat_raw)
dat_melt <- melt(data = dat_clean,
                 id.vars = c("iso3c","Country"),
                 variable.name = "year") %>%
  mutate(value = gsub("xxx", NA, value),
         value = as.numeric(value)) %>%
  filter(Country != "Yemen, North")

dat_final <- dat_melt %>%
  mutate(year = as.integer(as.character(year))) %>%
  drop_na() %>%
  filter(year >= 2000)

dat_final$variablename <- "Military expenditure"
dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)

check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT) # this is different from other scripts
check_coverage(dat_final, "iso3c_CONVERT", "year", SCRIPT)
check_duplicates(dat_final, "iso3c_CONVERT", "year", "variablename", SCRIPT)
check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c, year, value, variablename)

raw.data$milex <- dat_final
rmExcept("raw.data")