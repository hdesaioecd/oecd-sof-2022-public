source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "28-social-protection"

# https://unstats.un.org/sdgs/dataportal - indicator 1.3.1
dat_raw <-
  read_excel("./data/2020 sfr model data/SDG_0131_SEX_SOC_RT_A_EN.xlsx",
             skip = 5) %>%
  janitor::clean_names() 
#capturing population covered by at least one social protection benefit
dat_clean <- setDT(dat_raw)[sex == "Total",
                            .(iso3c = reference_area,
                              year = as.integer(time),
                              value = population_covered_by_at_least_one_social_protection_benefit)]
dat_clean$iso3c <- country.code.name(dat_clean$iso3c)
dat_final <- na.omit(dat_clean)

# NEW
dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)
dat_final$variablename <- "Social protection coverage"

# Adding the checks
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final %>% mutate(iso3c = country.code.name(iso3c)),"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", NULL,SCRIPT)

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c,year,value,variablename)
check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this

raw.data$socialprotection <- dat_final
rmExcept("raw.data")