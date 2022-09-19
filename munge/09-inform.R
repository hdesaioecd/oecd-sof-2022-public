source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "09-INFORM"

filename <-
  "./data/2020 sfr model data/INFORM2022_TREND_2012_2021_v062_ALL.xlsx"
inform_raw <- read_excel(filename) %>% clean_names()
indicators <- raw.data$log %>% dplyr::filter(source == "INFORM",
                                             include == 1)

#always inform year - 2
inform_clean <- setDT(inform_raw)[indicator_type == "INORM Index" &
                                    indicator_name %in% unique(indicators$variablename),
                                  .(
                                    iso3c = iso3,
                                    value = indicator_score,
                                    variablename = indicator_name,
                                    year = inform_year - 2 #subtract by 2 based on standard practice of survey/reporting year
                                  )]

inform_clean$iso3c_CONVERT <- country.code.name(inform_clean$iso3c)

check_convert_iso(inform_clean, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(inform_clean, "iso3c", "year", SCRIPT)
check_duplicates(inform_clean, "iso3c", "year", "variablename", SCRIPT)
check_nas(inform_clean, SCRIPT)

inform_clean$iso3c = inform_clean$iso3c_CONVERT
inform_clean = inform_clean %>% select(iso3c, year, value, variablename)

raw.data$inform <- inform_clean
rmExcept("raw.data")