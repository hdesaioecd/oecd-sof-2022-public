source("./lib/funcs.R")
source("./lib/888-logging_unit-tests.R")

SCRIPT = "23-forced-displacement-by-asylum-country"

#calculate refugees and IDPs by country of asylum 
#in relation to social cohesion, so including IDPs is fine, but need to source it from IDMC rather than UNHCR)
unhcr_raw <-
  fread("./data/2020 sfr model data/forced displacement/unhcr_asylum.csv",
        skip = 14) %>%
  janitor::clean_names() %>%
  dplyr::select(-c(
    country_of_origin,
    country_of_origin_iso,
    country_of_asylum_iso
  )) %>%
  tidyr::pivot_longer(names_to = "indicator",
                      values_to = "value",
                      -c(year,
                         country_of_asylum)) %>%
  dplyr::rename(iso3c = country_of_asylum) %>%
  filter(indicator != "id_ps_of_concern_to_unhcr")

unrwa_raw <-
  fread("./data/2020 sfr model data/forced displacement/unrwa_asylum.csv",
        skip = 14) %>%
  clean_names() %>%
  select(-c(
    country_of_origin,
    country_of_origin_iso,
    country_of_asylum_iso
  )) %>%
  rename(value = total,
         iso3c = country_of_asylum) %>%
  mutate(indicator = "unrwa_refugees")

idmc_raw <-
  read_excel(
    "./data/2020 sfr model data/IDMC_Internal_Displacement_Conflict-Violence_Disasters_2008_2021.xlsx",
    "Displacement data"
  ) %>%
  dplyr::select(
    iso3c = Name,
    disaster_idp = `Disaster Internal Displacements`,
    conflict_idp = `Conflict Internal Displacements`,
    year = Year
  ) %>%
  slice(-1) %>%
  pivot_longer(names_to = "indicator",
               values_to = "value",-c(iso3c,year)) %>%
  mutate(value = as.numeric(value),
         value = replace_na(value,0))

displacement_merged <- rbind(unhcr_raw, unrwa_raw, idmc_raw)

displacement_merged_agg <- displacement_merged %>%
  mutate(iso3c = country.code.name(iso3c),
         year = as.numeric(year)) %>%
  group_by(iso3c, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
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

dat_final <- displacement_merged_agg %>%
  left_join(pop_final, c("iso3c", "year")) %>%
  mutate(value = value / population) %>%
  select(iso3c, year, value) %>%
  ungroup()

dat_final <- as_tibble(dat_final)
dat_final$variablename <- "Forced displacement"
dat_final <-
  add.zeros.for.missing.countries(dat_final,
                                  raw.data) #adds zero for countries with no observations
dat_final <-
  extend.time.series(dat_final,
                     replace.with = 0) #adds zero for years with no observations

#NEW
dat_final$iso3c_CONVERT <- country.code.name(dat_final$iso3c)

check_convert_iso(dat_final, iso3c, iso3c_CONVERT, SCRIPT)
check_coverage(dat_final,"iso3c", "year", SCRIPT)
check_duplicates(dat_final,"iso3c", "year", "variablename",SCRIPT)

dat_final$iso3c = dat_final$iso3c_CONVERT
dat_final = dat_final %>% select(iso3c, year, value, variablename)

check_nas(dat_final, SCRIPT) # drop the iso3c convert before running this

raw.data$displacement_asylum <- dat_final
rmExcept("raw.data")