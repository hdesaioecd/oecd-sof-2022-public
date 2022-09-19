source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

#load filename
filename <- "./data/2020 sfr model data/CPI2021_GlobalResults&Trends.xlsx"

SCRIPT = "07-corruption"
ti_raw <- read_excel(filename, 
                     sheet = 2, 
                     skip = 2)

#clean column names and keep all identifier columns and ones where the name contains "score"
ti_clean <-
  ti_raw %>% 
  janitor::clean_names() %>% 
  dplyr::select(iso3c = iso3, contains("score"))

#turn dataset into long format
ti_long <- ti_clean %>% pivot_longer(names_to = "year",
                                     values_to = "value", 
                                     -iso3c)

ti_long <- na.omit(ti_long)
ti_long$year <- gsub("cpi_score_","",ti_long$year)

#get rid of prefix in year column
ti_long$year <- as.integer(ti_long$year)

ti_long$iso3c_CONVERT <- country.code.name(ti_long$iso3c)
ti_long = as.data.frame(ti_long)

check_convert_iso(ti_long, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(ti_long,"iso3c", "year", SCRIPT)
check_duplicates(ti_long,"iso3c", "year",NULL ,SCRIPT)
check_nas(ti_long, SCRIPT)

ti_long$variablename <- "Corruptions Perception"
ti_long = ti_long %>% mutate(iso3c = iso3c_CONVERT)
ti_long = ti_long %>% select(iso3c,year,value,variablename)

raw.data$ti <- ti_long
rmExcept("raw.data")