source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

# https://www.internal-displacement.org/global-report/grid2022/

SCRIPT = "21-environmental-displacement"

file_path <-
  "./data/2020 sfr model data/IDMC_Internal_Displacement_Conflict-Violence_Disasters_2008_2021.xlsx"
dat_raw <- read_excel(file_path, "Displacement data")

dat_clean <- dat_raw %>%
  dplyr::select(iso3c = Name,
                value = `Disaster Internal Displacements`,
                year = Year) %>%
  slice(-1) %>%
  mutate(
    variablename = "Environment-related displacement",
    iso3c = country.code.name(iso3c),
    year = as.integer(year),
    value = as.numeric(value)
  ) %>%
  drop_na()

# population adjustment
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

pop_func <- function(x) {
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
                        values_to = "value", -c(iso3c)) %>%
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

pop_final <- rbind(pop_est_clean, pop_proj_clean)
check_duplicates(pop_final, "iso3c", "year", NULL, SCRIPT)

dat_final <- dat_clean %>%
  left_join(pop_final, c("iso3c", "year")) %>%
  mutate(value = value / population) %>%
  select(-population)

#add zeros for missing countries
dat_final <- add.zeros.for.missing.countries(dat_final,
                                             raw.data) #warnings are fine - coercing factor into character
#extend time series
dat_final <- extend.time.series(dat_final, replace.with = 0)

dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)
check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final, "iso3c", "year", SCRIPT)
check_duplicates(dat_final, "iso3c", "year", "variablename", SCRIPT)

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>%
  select(iso3c, year, value, variablename)

check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this

raw.data$env_displacement <- dat_final
rmExcept("raw.data")