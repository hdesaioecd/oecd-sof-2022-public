source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "02-exchange-rate volatility"

# Domestic currency per USD -----------------------------------------------

#calculate exchange rate volatility using domestic currency per USD from IMF via WB
dat_path <-
  "./data/2020 sfr model data/API_PA.NUS.FCRF_DS2_en_csv_v2_4151802.csv"
dat_raw <- fread(dat_path,header = T) %>%
  wb.func()

# NEW conversion with iso3c convert for logging
dat_raw$iso3c_CONVERT <- country.code.name(dat_raw$iso3c)

# INSERTING THE CHECKS
check_convert_iso(dat_raw, iso3c, iso3c_CONVERT, SCRIPT)
check_duplicates(dat_raw, "iso3c", "year", NULL, SCRIPT)
check_nas(dat_raw, SCRIPT)

dat_raw$variablename <- "Exchange rate volatility"
#need to use extend time series function here so that R doesn't mistakenly
#calculate rolling function across a break in years
#assign these years NA values
dat_clean <-
  extend.time.series(dat_raw, replace.with = NA)

# TO DO
check_coverage(dat_clean, "iso3c", "year", SCRIPT) # doesnt work with factors

#calculate log, grouped by year
#this code is pedantic, but - ungroup, then arrange, and then group by iso3c
#to calculate three-year rolling SD of logged value
#this is your measure of volatility
dat_final <- dat_clean %>%
  group_by(year) %>%
  mutate(value = log(value)) %>%
  ungroup() %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(value = roll_sd(value, 3))

dat_final <- na.omit(dat_final)
check_coverage(dat_final)
dat_final = dat_final %>%
  select(iso3c, year, value, variablename)

raw.data$exchangerate <- dat_final
rmExcept("raw.data")