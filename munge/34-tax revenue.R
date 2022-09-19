source('./lib/funcs.R')
source("./lib/888-logging_unit-tests.R")

library(readstata13)

SCRIPT = "34-tax-revenue"

#import stata dataframe of ICTD tax revenue dataset: https://www.ictd.ac/dataset/grd/
dat_raw <- readstata13::read.dta13("./data/2020 sfr model data/Merged.dta")
#use tax revenues excluding social contributions
dat_clean <- setDT(dat_raw)[, .(iso3c = country,
                                year,
                                value = tax_ex_sc,
                                variablename = "Tax revenue")][complete.cases(value)]
dat_clean$iso3c <- country.code.name(dat_clean$iso3c)

# NEW
dat_clean$iso3c_CONVERT <- country.code.name(dat_clean$iso3c)

# Adding the checks
check_convert_iso(dat_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_clean,"iso3c", "year", SCRIPT)
check_duplicates(dat_clean,"iso3c", "year", "variablename",SCRIPT)

dat_clean$iso3c = dat_clean$iso3c_CONVERT
dat_clean = dat_clean %>% select(iso3c,year,value,variablename)
check_nas(dat_clean, SCRIPT) 

raw.data$tax <- dat_clean
rmExcept("raw.data")