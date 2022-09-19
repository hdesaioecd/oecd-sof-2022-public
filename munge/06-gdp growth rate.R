source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "06-gdp growth rate"

dat_raw <-
  fread(
    "./data/2020 sfr model data/API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_4150725.csv",
    header = T
  ) %>%
  wb.func()

dat_raw$variablename <- "GDP per capita growth rate"
dat_clean <- extend.time.series(dat_raw,
                                replace.with = NA)
#insert logging checks
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_duplicates(dat_clean, "iso3c", "year", NULL, SCRIPT)
check_nas(dat_clean, SCRIPT)
check_coverage(dat_clean, "iso3c", "year", SCRIPT) # doesnt work with factors

dat_final <- dat_clean %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(value = roll_mean(value, 3)) %>%
  filter(year >= 2000)

dat_final <- na.omit(dat_final)
check_coverage(dat_final, "iso3c", "year", SCRIPT)
dat_final = dat_final %>%
  select(iso3c, year, value, variablename)

raw.data$gdpgrowth <- dat_final
rmExcept("raw.data") 