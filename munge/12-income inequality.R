source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "12-income inequality"

dat_raw <-
  read_excel("./data/2020 sfr model data/wiidcountry_0_0.xlsx")

dat_clean <-
  setDT(dat_raw)[year >= 2000, .(iso3c = country.code.name(country),
                                 year,
                                 value = palma,
                                 variablename = "Income inequality"
                                 )
                 ]
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

# TO DO - bespoke
dat_clean = dat_clean %>%
  filter(!is.na(iso3c))
#removes Serbia and Montenegro, which is fine because of earlier years

check_convert_iso(dat_clean, iso3c_CONVERT, iso3c,SCRIPT) # this is different from other scripts due to starting with country name rather than iso code 
check_coverage(dat_clean,"iso3c_CONVERT", "year", SCRIPT)
check_duplicates(dat_clean,"iso3c", "year", "variablename",SCRIPT)
check_nas(dat_clean, SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)

dat_clean <- na.omit(dat_clean)
raw.data$palma <- dat_clean
rmExcept("raw.data")
