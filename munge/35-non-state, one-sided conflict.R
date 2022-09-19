source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "35-non-state-one-sided-conflict"

# import UCDP dataset, converted to parquet format (using arrow), with 2021 as the latest year
# source: https://ucdp.uu.se/downloads/
#dat_raw <- read_parquet("./data/2020 sfr model data/ged211.parquet")
dat_raw <- read_parquet("./data/2020 sfr model data/ged221.parquet")
dat_clean <-
  setDT(dat_raw)[type_of_violence != 1,
                 .(value = sum(best, na.rm = T)),
                 .(year,
                   country)]
dat_clean$iso3c <- country.code.name(dat_clean$country)
dat_clean$country <- NULL

# Per capita estimates ----------------------------------------------------
# using 2018 UNDESA World Population Prospects, estimates (up to 2020)
# per 100k population
pop_est_raw <-
  read_excel(
    "./data/population data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
    sheet = "ESTIMATES",
    skip = 16
  ) 

pop_proj_raw <-
  read_excel(
    "./data/population data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
    sheet = "MEDIUM VARIANT",
    skip = 16
  ) 

pop_func <- function(x){
  x %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "Country/Area") %>%
    dplyr::mutate(iso3c = country.code.name(region_subregion_country_or_area)) %>%
    dplyr::select(
      -c(
        index,
        variant,
        region_subregion_country_or_area,
        notes,
        country_code,
        type,
        parent_code
      )
    ) %>%
    tidyr::pivot_longer(names_to = "year",
                        values_to = "value",-c(iso3c)) %>%
    dplyr::mutate(
      year = gsub("x", "", year),
      year = as.integer(year),
      value = as.numeric(value) * 1000 / 100000 #per 100,000 population
    ) %>%
    rename(population = value)
}

pop_est_clean <- pop_est_raw %>%
  pop_func()

pop_proj_clean <- pop_proj_raw %>%
  pop_func() %>%
  filter(year == 2021)

pop_final <- rbind(pop_est_clean,pop_proj_clean)
check_duplicates(pop_final,"iso3c","year",NULL,SCRIPT)

dat_final <- dat_clean %>%
  left_join(pop_final,
            c("year", "iso3c")) %>%
  mutate(value = value / population) %>%
  select(-population) %>%
  arrange(iso3c, year)

# address outliers here by taking the log (as in the battle-related deaths data)
max_score = max(dat_final$value)
log_base = (max_score+1)^(1/10)
dat_final$value <- log(dat_final$value + 1, log_base)
dat_final <- as_tibble(dat_final)
dat_final$variablename <- "non_state_one_sided"

#### add zeros for missing values
dat_final <-
  add.zeros.for.missing.countries(dat_final, 
                                  raw.data) #adds zero for countries with no observations
dat_final <-
  extend.time.series(dat_final, 
                     replace.with = 0) #adds zero for years with no observations

dat_final <- dat_final %>%
  group_by(iso3c) %>%
  mutate(value = roll_mean(value,3)) %>%
  drop_na(value)

# NEW
dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)

# Adding the checks
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final,"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", "variablename",SCRIPT)
dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(-c(iso3c_CONVERT))
dat_final = dat_final %>% select(iso3c,year,value,variablename)

check_nas(dat_final, SCRIPT) 

raw.data$ucdp <- dat_final
rmExcept("raw.data") 