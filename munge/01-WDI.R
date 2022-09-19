source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "01-WDI"

#import list of indicators from log file where source = 1 WDI and include = 1
#use these to filter the main WDI bulk download
indicators <- setDT(raw.data$log)[source == "WDI" &
                                    include == 1][["variablename"]]
#read in parquet file
dat_raw <-
  arrow::read_parquet("./data/2020 sfr model data/WDIData.parquet") %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names()

#filter indicators here using data.table
dat_clean <- setDT(dat_raw)[indicator_name %in% indicators]
# TO DO - bespoke

#change from wide to long
dat_long <- melt(
  data = dat_clean,
  id.vars = c(
    "country_name",
    "country_code",
    "indicator_name",
    "indicator_code"
  ),
  variable.name = "year",
  values.name = "value"
)

dat_final <- dat_long[complete.cases(value),
                      .(
                        iso3c = country_name,
                        iso3c_CONVERT = country.code.name(country_name),
                        year = as.integer(gsub("x", "", year)),
                        variablename = indicator_name,
                        value
                      )]

check_convert_iso(dat_final, iso3c_CONVERT, iso3c, SCRIPT)
check_coverage(dat_final, "iso3c", "year", SCRIPT)
check_duplicates(dat_final, "iso3c", "year", "variablename", SCRIPT)
check_nas(dat_final, SCRIPT)
#check_nas indicates that there are many NA values for iso3_CONVERT. That's fine because those values
#represent regional aggregates.

dat_final = dat_final %>% select(iso3c, year, value, variablename)
raw.data$wdi <- dat_final
rmExcept("raw.data")