source("./lib/funcs.R")
`%!in%` = Negate(`%in%`)
output_folders <- c("./data_out2022/")
library(patchwork)

# Export time series ------------------------------------------------------

sfr.time.series_export <- sfr.time.series %>%
  dplyr::select(iso3c, year, variablename, dimension, value) %>%
  split(.$dimension) %>%
  purrr::map(
    function(x)
      x %>%
      group_by(variablename) %>%
      pivot_wider(names_from = "variablename",
                  values_from = "value") %>%
      select(-dimension) %>%
      left_join(final_results_output %>% select(iso3c, category = type), "iso3c")
  )

write.xlsx(
  sfr.time.series_export %>%
    map(
      function(x)
        x %>%
        mutate(country = country.code.name(iso3c)) %>%
        select(iso3c, country, everything()) %>%
        arrange(-year)
    ),
  paste0(
    output_folders,
    "./Results/Export of indicators (with imputed values).xlsx"
  ),
  overwrite = T
)

# Clusters ----------------------------------------------------------------

#adjust and export clusters
#incorporate fragile.levels, the qual decisions on the clusters
fragile.levels <-
  import("./data/additional data/dimensional fragility.xlsx")

clusters <-
  clusters %>% dplyr::mutate(country = country.code.name(iso3c))

colours <-
  mean.plot %>% dplyr::rename(color = colour) %>% dplyr::select(cluster, color) %>% distinct()

clusters_adj <-
  clusters %>%
  dplyr::left_join(colours, by = "cluster") %>%
  dplyr::select(dimension, iso3c, country, color) %>%
  arrange(dimension, color, iso3c) %>%
  filter(dimension != "Aggregate")

clusters_split <- clusters_adj %>%
  split(.$dimension)

countries_split <- clusters_adj %>%
  mutate(country = oecd.country.name(iso3c)) %>%
  mutate(
    country = gsub("Democratic People’s Republic of Korea",
                   "DPRK", country),
    country = gsub("Democratic Republic of the Congo",
                   "DRC", country),
    country = gsub("Former Yugoslav Republic of Macedonia",
                   "Macedonia", country),
    country = gsub("Lao People’s Democratic Republic",
                   "Lao PDR",
                   country)
  ) %>%
  left_join(fragile.levels, c("color", "dimension")) %>%
  split(.$iso3c)

write.xlsx(
  clusters_split %>% map(function(x)
    x %>%
      select(country, color)),
  paste0(output_folders, "clusters/clusters by dimension.xlsx"),
  overwrite = T
)

write.xlsx(
  countries_split %>% map(function(x)
    x %>% select(iso3c, dimension, description)),
  paste0(output_folders, "clusters/clusters by countries.xlsx"),
  overwrite = T
)

clusters_export <- clusters %>%
  left_join(fragile.levels, c("dimension", "cluster")) %>%
  filter(dimension != "Aggregate")

#total count of countries in each cluster
clusters_total_count <- clusters_export %>%
  group_by(dimension, description) %>%
  mutate(
    description = sub(" .*", "", description),
    description = factor(
      description,
      levels = c("Severe",
                 "High",
                 "Moderate",
                 "Low",
                 "Minor")
    )
  ) %>%
  summarise(n = n())

write.xlsx(
  clusters_total_count %>% pivot_wider(names_from = description,
                                       values_from = n),
  paste0(output_folders,
         "./clusters/clusters by count.xlsx"),
  overwrite = T
)

write.xlsx(
  clusters_export,
  paste0(output_folders, "clusters/Clusters with Colors.xlsx"),
  overwrite = T
)

clusters_export_fragile <- final_results_output %>%
  filter(type != "Rest of the world") %>%
  select(iso3c) %>%
  left_join(clusters_export, "iso3c")

clusters_export_for_designer <- clusters_export_fragile %>%
  mutate(shading = ifelse(
    grepl("Severe", description),
    "Darkest Shade",
    ifelse(
      grepl("High", description),
      "Second-darkest shade",
      ifelse(
        grepl("Moderate", description),
        "Third-darkest shade",
        ifelse(
          grepl("Low", description),
          "Fourth-darkest shade",
          "Lightest shade"
        )
      )
    )
  ),
  country = oecd.country.name(iso3c)) %>%
  select(iso3c, country, dimension, shading) %>%
  pivot_wider(names_from = "dimension", values_from = "shading")

write.xlsx(
  clusters_export_for_designer,
  paste0(output_folders, "clusters/Clusters for Designer.xlsx"),
  overwrite = T
)

# Country Breakdown by Groups ---------------------------------------------

#population - now and projected
pop_path <-
  "./data/population data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"

population_hist <- read_excel(pop_path,
                              "ESTIMATES",
                              skip = 16) %>%
  population_func()

population_proj <- read_excel(pop_path,
                              "MEDIUM VARIANT",
                              skip = 16) %>%
  population_func() %>%
  filter(year != 2020)

#adjust to millions
population_cleaned <- rbind(population_hist, population_proj) %>%
  mutate(population = population / 1000000) %>%
  select(iso3c, year, population)

#population - global
pop_global_func <- function(x) {
  x %>%
    filter(Type == "World") %>%
    select(-c(1, 2, 4:7)) %>%
    pivot_longer(names_to = "year",
                 values_to = "global",-1) %>%
    select(-1) %>%
    mutate(global = as.numeric(global) * 1000 / 1000000,
           year = as.integer(year))
}

population_world_estimates <- read_excel(pop_path,
                                         "ESTIMATES",
                                         skip = 16) %>%
  pop_global_func()

population_world_proj <- read_excel(pop_path,
                                    "MEDIUM VARIANT",
                                    skip = 16) %>%
  pop_global_func()

population_world <-
  rbind(population_world_estimates, population_world_proj)

population_world_2022 <- population_world %>%
  filter(year == 2022)

##import list of ODA eligible recipients - should amount to 142
oecd_raw <-
  read_excel("./data/additional data/DAC-CRS-CODES.xlsx", sheet = "Recipient") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  dplyr::mutate(iso3c = country.code.name(recipient_name_en))

oecd_classes <- oecd_raw %>%
  dplyr::filter(complete.cases(iso3c),
                dac_income_group != "Part I unallocated") %>%
  dplyr::select(iso3c, region, dac_income_group) %>%
  dplyr::mutate(category = "ODA eligible")

setDT(oecd_classes)
oda_recipients <- oecd_classes[, .(iso3c, group = category)]
oecd_regions <- oecd_classes[, .(iso3c, group = region)]
oecd_income <- oecd_classes[, .(iso3c, group = dac_income_group)]
oecd_landlocked <-
  setDT(oecd_raw)[land_locked == 1 & complete.cases(iso3c), .(iso3c,
                                                              group = "Landlocked")]
oecd_sids <-
  setDT(oecd_raw)[sids == 1 & complete.cases(iso3c), .(iso3c,
                                                       group = "SIDS")]
oecd_class <- rbind(oecd_regions, oecd_income,
                    oecd_landlocked,
                    oecd_sids) %>%
  mutate(group = paste0(group, " - OECD classes"))

#oecd m49 codes
#un_m49_codes <- fread("./data/additional data/UNSD - Methodology.csv") %>%

un_m49_codes <-
  fread("./data/additional data/UNSD_Methodology.csv") %>%
  as.data.frame() %>%
  #rio::import("./data/additional data/UNSD — Methodology.csv") %>%
  janitor::clean_names() %>%
  rename(iso3c = iso_alpha3_code)
#as.data.frame() %>% # trying to fix the renaming issue
#un_m49_codes = un_m49_codes %>% as.data.frame() %>% rename(iso3c = "iso_alpha3_code") # originally not in quotation

un_m49_clean <- oecd_regions %>%
  select(-group) %>%
  left_join(un_m49_codes, "iso3c")

un_m49_agg_reg <- un_m49_clean %>%
  select(iso3c, group = region_name) %>%
  mutate(
    group = ifelse(iso3c == "XKX",
                   "Europe", group),
    group = paste0("OECD Region - ", group)
  )

un_m49_agg_subreg <- un_m49_clean %>%
  select(iso3c, group = sub_region_name) %>%
  mutate(
    group = ifelse(iso3c == "XKX",
                   "Eastern Europe", group),
    group = paste0("OECD Subregion - ", group)
  )


#create list for all fragile contexts
oecd_fragile_all <- final_results_output %>%
  filter(type %in% c("Extremely fragile",
                     "Other fragile")) %>%
  mutate(type = "Fragile") %>%
  select(iso3c, group = type)

#create lists limited to the ODA eligible recipients
oecd_nonfragile <- oda_recipients %>%
  dplyr::filter(iso3c %!in% unique(oecd_fragile_all$iso3c)) %>%
  dplyr::mutate(group = "Rest of the world (ODA-eligible)") %>%
  dplyr::select(iso3c, group)

#create list separating other and extremely fragile contexts
oecd_fragile_disag <- final_results_output %>%
  filter(type %in% c("Extremely fragile",
                     "Other fragile")) %>%
  select(iso3c, group = type)

#import WB 2020 June classifications, and use Groups spreadsheet
wb_class <-
  read_excel("./data/additional data/CLASS.xlsx",
             sheet = "Groups") %>%
  dplyr::mutate(iso3c = country.code.name(CountryName)) %>%
  dplyr::select(iso3c, group = GroupName) %>%
  dplyr::mutate(group = paste0(group, " - WB classes"))

#import custom list of sub-regions, developed for SoF 2020
subregions <-
  read_excel("./data/additional data/List of sub-regions.xlsx") %>%
  dplyr::mutate(iso3c = countrycode(countries, "country.name", "iso3c"))

##import custom list of resource-dependent economies from UNCTAD 2019
commodities_list <-
  read_excel("./data/additional data/commodity.xlsx") %>%
  rename(group = commodity)

commodities_all <-
  commodities_list %>%
  mutate(group = "Commodity dependent")

commodities_disaggregated <-
  commodities_list %>%
  select(iso3c, group) %>%
  mutate(group = paste0("Commodity dependence - ", group))

# Conflict vs.  fragility analysis -------------------------------------------------

conflict_raw <-
  read_parquet("./data/2020 sfr model data/ged221.parquet")
conflict_list <- setDT(conflict_raw)[year == max(year) &
                                       type_of_violence == 1 &
                                       active_year == 1,
                                     .(value = sum(best, na.rm = T)),
                                     country][value >= 25]
conflict_list$iso3c <- country.code.name(conflict_list$country)

conflict_list_all <- conflict_list[, .(iso3c,
                                       group = "Conflict-affected")]
conflict_list_minor <- conflict_list[value >= 25 &
                                       value < 1000,
                                     .(iso3c,
                                       group = "Low-intensity armed conflict")]
conflict_list_major <- conflict_list[value >= 1000,
                                     .(iso3c,
                                       group = "High-intensity armed conflict")]

conflict_episodes <- conflict_raw[type_of_violence == 1 &
                                    active_year == 1,
                                  .(value = sum(best,
                                                na.rm = T)),
                                  .(year,
                                    country)][value >= 25]
conflict_episodes$iso3c <-
  country.code.name(conflict_episodes$country)
conflict_episodes_nonhighintensity <-
  conflict_episodes[iso3c %!in% unique(conflict_list_major$iso3c)][, experienced_conflict := if_else(value >= 1000,
                                                                                                     "TRUE",
                                                                                                     "FALSE")][order(year)]
conflict_episodes_nonhighintensity_wide <-
  conflict_episodes_nonhighintensity %>%
  select(-c(value)) %>%
  pivot_wider(names_from = "year",
              values_from = "experienced_conflict")


conflict_amounts <- conflict_raw[active_year == 1,
                                 .(value = sum(best, na.rm = T)),
                                 .(type_of_violence,
                                   conflict_new_id,
                                   year)][value >= 25][, .N,
                                                       .(year,
                                                         type_of_violence)]
conflict_raw$iso3c <- country.code.name(conflict_raw$country)
conflict_amounts_fragile <- conflict_raw[active_year == 1 &
                                           iso3c %in% unique(oecd_fragile_all$iso3c),
                                         .(value = sum(best, na.rm = T)),
                                         .(type_of_violence,
                                           conflict_new_id,
                                           year)][value >= 25][,
                                                               .N,
                                                               .(year,
                                                                 type_of_violence)][conflict_amounts,
                                                                                    on = c("year",
                                                                                           "type_of_violence")][, prop := N /
                                                                                                                  i.N * 100][order(year)]
write.xlsx(
  conflict_amounts_fragile %>%
    select(year, type_of_violence, prop) %>%
    pivot_wider(names_from = year,
                values_from = prop),
  "./graphs/conflict/conflict intensity in fragile contexts.xlsx",
  overwrite = T
)

#conflict fatalities
conf_fatal_total <-
  conflict_raw[active_year == 1,
               .(total = sum(best, na.rm = T)),
               .(year,
                 type_of_violence)][order(year)]

conf_fatal_frag <-
  conflict_raw[active_year == 1 &
                 iso3c %in% unique(oecd_fragile_all$iso3c),
               .(fragile = sum(best, na.rm = T)),
               .(year,
                 type_of_violence)][order(year)]

conf_fatal_comp <- conf_fatal_frag %>%
  left_join(conf_fatal_total,
            c("year",
              "type_of_violence")) %>%
  mutate(proportion = fragile / total * 100) %>%
  select(-c(fragile,
            total)) %>%
  pivot_wider(names_from = year,
              values_from = proportion)
write.xlsx(
  conf_fatal_comp,
  "./graphs/conflict/fatalities in fragile contexts.xlsx",
  overwrite = T
)

#regimes
regime_raw <-
  fread("./data/2020 sfr model data/political-regime.csv") %>%
  clean_names() %>%
  filter(year == max(year)) %>%
  mutate(
    regime_row_owid = case_when(
      regime_row_owid == 0 ~
        "Closed autocracies",
      regime_row_owid == 1 ~
        "Electoral autocracies",
      regime_row_owid == 2 ~
        "Electoral democracies",
      regime_row_owid == 3 ~
        "Liberal democracies"
    )
  ) %>%
  mutate(iso3c = country.code.name(entity),
         iso3c = ifelse(entity == "Timor", "TLS", iso3c)) %>%
  select(iso3c,
         group = regime_row_owid) %>%
  drop_na(iso3c)

# Other analysis ----------------------------------------------------------

#import custom list of climate change countries, from ND-GAIN, using thresholds proposed in Krampe 2019 peacebuilding and climate change brief
nd_gain_raw <-
  fread("./data/additional data/resources/vulnerability/exposure.csv",
        header = T) %>%
  clean_names() %>%
  mutate(iso3c = country.code.name(name),
         rank = rank(x2019))

climate_change <- nd_gain_raw %>%
  filter(rank > 144) %>%
  mutate(group = "Most exposed to climate change") %>%
  select(iso3c, group)

climate_change_frag <- climate_change %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

conflict_and_climate <-
  conflict_list_all %>%
  filter(iso3c %in% unique(climate_change$iso3c)) %>%
  mutate(group = "Conflict-affected and highly exposed to climate change")

war_and_climate <-
  conflict_list_major %>%
  filter(iso3c %in% unique(climate_change$iso3c)) %>%
  mutate(group = "In war and highly exposed to climate change")


#eiu
eiu_raw <-
  fread(
    "./data/additional data/tabula-eiu-democracy-index-2021.csv",
    skip = 4,
    fill = TRUE
  ) %>%
  select(country = V1,
         score = V2) %>%
  mutate(score = as.numeric(score)) %>%
  drop_na() %>%
  mutate(group = ifelse(
    score > 8.00,
    "Full democracy",
    ifelse(
      score > 6.00,
      "Flawed democracy",
      ifelse(score > 4.00,
             "Hybrid regime",
             "Authoritarian")
    )
  ),
  iso3c = country.code.name(country)) %>%
  select(iso3c, group)

#PBSO conflict-affected
pbso_raw <-
  fread("./data/additional data/pbso_conflict_affected.csv") %>%
  select(iso3c) %>%
  mutate(group = "Conflict-affected (PBSO)")

#rbind entire list
iso_bygroup <-
  rbind(
    oecd_class,
    wb_class,
    oecd_fragile_disag,
    oecd_fragile_all,
    oecd_nonfragile,
    subregions %>% dplyr::select(-c(countries, source)),
    oda_recipients,
    commodities_all,
    commodities_disaggregated,
    conflict_list_all,
    conflict_list_minor,
    conflict_list_major,
    pbso_raw,
    climate_change,
    conflict_and_climate,
    war_and_climate,
    eiu_raw,
    regime_raw,
    clusters_export %>% select(iso3c, group = description),
    un_m49_agg_reg,
    un_m49_agg_subreg
  ) %>%
  distinct()

#summarise the number of countries per group
iso_counts <- iso_bygroup %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(count_total = n())

#subset the isogroup list to only include fragile contexts
iso_counts_fragile <- iso_bygroup %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

#compute count of number of fragile states per group,
#then join the previous list of total countries per group
#to determine proportion of group that is fragile
iso_counts_fragile_bygroup <- iso_counts_fragile %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(count_fragile = n()) %>%
  dplyr::left_join(iso_counts, by = "group") %>%
  dplyr::mutate(proportion = count_fragile / count_total * 100)

iso_counts_wpop <- iso_bygroup %>%
  dplyr::left_join(
    population_cleaned %>%
      dplyr::filter(year == 2021) %>%
      dplyr::select(iso3c, population),
    by = "iso3c"
  ) %>%
  drop_na()

iso_counts_wpop_bygroup <- iso_counts_wpop %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(population_all = sum(population, na.rm = T))

iso_counts_wpop_fragile <- iso_counts_wpop %>%
  dplyr::left_join(oecd_fragile_all %>% rename(type = group), by = "iso3c") %>%
  drop_na(type)

pop_fragile <- iso_counts_wpop_fragile %>%
  filter(group == "Fragile") %>%
  summarise(total_frag_pop = sum(population, na.rm = T))

iso_counts_wpop_fragile_bygroup <- iso_counts_wpop_fragile %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(population_fragile = sum(population)) %>%
  dplyr::left_join(iso_counts_wpop_bygroup, by = "group") %>%
  dplyr::mutate(
    proportion = population_fragile / population_all * 100,
    total_frag_pop = pop_fragile$total_frag_pop,
    proportion_of_frag = population_fragile / total_frag_pop *
      100,
    fragile_proportion_of_world = population_fragile / population_world_2022$global * 100
  )

iso_counts_export <-
  list(
    "Countries" = iso_counts_fragile_bygroup %>% dplyr::select(
      Groups = group,
      `Fragile Contexts` = count_fragile,
      `Total Contexts` = count_total,
      Proportion = proportion
    ),
    "Population" = iso_counts_wpop_fragile_bygroup %>%
      dplyr::mutate(
        population_fragile = round(population_fragile, 1),
        population_all = round(population_all, 1),
        proportion = round(proportion),
        proportion_of_frag = round(proportion_of_frag),
        fragile_proportion_of_world = round(fragile_proportion_of_world)
      ) %>%
      dplyr::select(
        Groups = group,
        `Population in Fragile Contexts in 2022` = population_fragile,
        `Total Population of Group in 2022` = population_all,
        Proportion = proportion,
        `Proportion of Fragile Contexts` = proportion_of_frag,
        `Subset in Fragile Contexts as a Proportion of the World` = fragile_proportion_of_world
      )
  )
write.xlsx(
  iso_counts_export,
  paste0(
    output_folders,
    "./Results/Country representation by group.xlsx"
  ),
  overwrite = T
)

#count by clusters
isoby_group_wo_clusters <- iso_bygroup %>%
  filter(!grepl("Fragility", group))

clusters_bygroup <- clusters_export %>%
  select(dimension, iso3c, description) %>%
  left_join(isoby_group_wo_clusters, "iso3c") %>%
  mutate(description = sub(" .*", "", description))

clusters_bygroup_agg <- clusters_bygroup %>%
  group_by(dimension, description, group) %>%
  summarise(n = n()) %>%
  left_join(clusters_total_count %>%
              rename(total = n),
            c("dimension", "description")) %>%
  mutate(prop = round(n / total * 100), ) %>%
  filter(group != "World - WB classes") %>%
  arrange(group)

clusters_bygroup_agg_prop <- clusters_bygroup_agg %>%
  select(-c(n, total)) %>%
  split(.$dimension) %>%
  map(function(x)
    (
      x %>%
        ungroup() %>%
        select(-dimension) %>%
        pivot_wider(names_from = description,
                    values_from = prop)
    ))

clusters_bygroup_agg_count <- clusters_bygroup_agg %>%
  select(-c(prop, total)) %>%
  split(.$dimension) %>%
  map(function(x)
    (
      x %>%
        ungroup() %>%
        select(-dimension) %>%
        pivot_wider(names_from = description,
                    values_from = n)
    ))

write.xlsx(
  clusters_bygroup_agg_count,
  paste0(
    output_folders,
    "./Results/Group representation by cluster - count.xlsx"
  ),
  overwrite = T
)
write.xlsx(
  clusters_bygroup_agg_prop,
  paste0(
    output_folders,
    "./Results/Group representation by cluster - proportions.xlsx"
  ),
  overwrite = T
)

#fragile subset of cluster representation by group
clusters_bygroup_agg_frag <- clusters_bygroup %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c)) %>%
  group_by(dimension, description, group) %>%
  summarise(n = n()) %>%
  left_join(clusters_total_count %>%
              rename(total = n),
            c("dimension", "description")) %>%
  mutate(prop = round(n / total * 100), ) %>%
  filter(group != "World - WB classes") %>%
  arrange(group)

clusters_bygroup_agg_prop_frag <- clusters_bygroup_agg_frag %>%
  select(-c(n, total)) %>%
  split(.$dimension) %>%
  map(function(x)
    (
      x %>%
        ungroup() %>%
        select(-dimension) %>%
        pivot_wider(names_from = description,
                    values_from = prop)
    ))

clusters_bygroup_agg_count_frag <- clusters_bygroup_agg_frag %>%
  select(-c(prop, total)) %>%
  split(.$dimension) %>%
  map(function(x)
    (
      x %>%
        ungroup() %>%
        select(-dimension) %>%
        pivot_wider(names_from = description,
                    values_from = n)
    ))

write.xlsx(
  clusters_bygroup_agg_count_frag,
  paste0(
    output_folders,
    "./Results/Group representation by cluster - count (fragile).xlsx"
  ),
  overwrite = T
)
write.xlsx(
  clusters_bygroup_agg_prop_frag,
  paste0(
    output_folders,
    "./Results/Group representation by cluster - proportions (fragile).xlsx"
  ),
  overwrite = T
)

# Top-line findings for the report ---------------------------------------

#population - fragile contexts, now and future
frag_pop <- population_cleaned %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

frag_pop_agg <- frag_pop %>%
  group_by(year) %>%
  summarise(value = sum(population, na.rm = T)) %>%
  left_join(population_world, "year") %>%
  mutate(share = round(value / global * 100)) %>%
  distinct()

frag_current_proj <- frag_pop_agg %>%
  filter(year %in% c(2022, 2030))

#poverty - using latest data available in IFS futures
pov_raw <- fread('./data/additional data/ifs-4.csv', header = T) %>%
  remove_empty("cols") %>%
  select(-c("V1", "V5", "V6")) %>%
  rename(country = V2) %>%
  pivot_longer(names_to = "year",
               values_to = "poverty", -country) %>%
  mutate(year = as.integer(year),
         iso3c = country.code.name(country)) %>%
  select(iso3c, country, year, poverty)

pov_global <- pov_raw %>%
  group_by(year) %>%
  summarise(global = sum(poverty, na.rm = T))

pov_frag <- oecd_fragile_all %>%
  select(-group) %>%
  left_join(pov_raw, "iso3c") %>%
  group_by(year) %>%
  summarise(fragile = sum(poverty, na.rm = T)) %>%
  left_join(pov_global, "year") %>%
  mutate(proportion = fragile / global * 100)

pov_filtered <- pov_frag %>%
  filter(year %in% c(2022, 2030))

#multidimensional poverty
multi_pov_raw <-
  read_excel(
    "./data/additional data/2020_mpi_statistical_data_table_1_and_2_en.xlsx",
    sheet = 1,
    skip = 4
  ) %>%
  remove_empty(c("cols", "rows")) %>%
  select(country  = Country, value = `(thousands)...13`) %>%
  drop_na() %>%
  mutate(
    iso3c = country.code.name(country),
    value = value * 1000 / 1000000,
    year = 2018
  )

multi_pov_dev <- multi_pov_raw %>%
  filter(country == "Developing countries") %>%
  select(-c(iso3c, country)) %>%
  rename(total = value)

multi_pov_frag <- multi_pov_raw %>%
  drop_na() %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c)) %>%
  group_by(year) %>%
  summarise(fragile = sum(value, na.rm = T)) %>%
  left_join(multi_pov_dev, "year") %>%
  mutate(proportion = fragile / total * 100)


# Cluster analysis of similarities ----------------------------------------

#cluster analysis
###if country has same level of fragility across all clusters
clust_simil <- clusters %>%
  left_join(fragile.levels, c("dimension", "cluster")) %>%
  select(iso3c, dimension, score = Fragility) %>%
  pivot_wider(names_from = dimension,
              values_from = score) %>%
  select(-Aggregate)

clust_simil_adj <- clust_simil %>%
  mutate(
    all_equal = if_else(
      Economic == Environmental & Environmental == Human &
        Human == Political &
        Political == Security & Security == Societal,
      TRUE,
      FALSE
    )
  )

clust_simil_adj_true <- clust_simil_adj %>%
  filter(all_equal == T) %>%
  mutate(country = oecd.country.name(iso3c)) %>%
  select(iso3c, country, everything()) %>%
  select(-all_equal) %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

###which countries have same level of fragility across all clusters
similarities <-
  clust_simil[(duplicated(clust_simil[c("Economic",
                                        "Environmental",
                                        "Human",
                                        "Political",
                                        "Security",
                                        "Societal")]) |
                 duplicated(clust_simil[c("Economic",
                                          "Environmental",
                                          "Human",
                                          "Political",
                                          "Security",
                                          "Societal")], fromLast = TRUE)), ] %>%
  mutate(country = oecd.country.name(iso3c)) %>%
  arrange(Economic, Environmental, Human, Political, Security, Societal) %>%
  select(iso3c, country, everything()) %>%
  filter(iso3c %in% unique(oda_recipients$iso3c))
write.xlsx(
  similarities,
  paste0(
    output_folders,
    "./clusters/Similarities across clusters.xlsx"
  ),
  overwrite = T
)

# Differences b/t 2020 and previous lists ---------------------------------

list_2018_raw <- fread("./data/additional data/list_2018.csv")
list_2018 <-
  list_2018_raw %>% dplyr::filter(year == 2017) %>%
  dplyr::select(iso3c, Aggregate.PC1) %>%
  dplyr::filter(Aggregate.PC1 < -1.20) %>%
  dplyr::mutate(country = country.code.name(iso3c)) %>%
  select(iso3c, country, score = Aggregate.PC1)

list_2016 <-
  fread("./data/additional data/list_2016.csv") %>%
  dplyr::filter(fragility.level %in% c("Fragile", "Extreme Fragility")) %>%
  dplyr::mutate(country = country.code.name(iso3c)) %>%
  select(iso3c, country, score = fragility)

list.2016.full <-
  fread(
    "https://raw.githubusercontent.com/githubIEP/oecd-sfr-2016/master/data_out/two-tier%20PCA%20-%20PC1%20as%20Fragility%20Score.csv"
  ) %>%
  select(iso3c, fragility)

list.2020.full <-
  fread(
    "https://raw.githubusercontent.com/hdesaioecd/oecd-sfr-2020-master-public/main/data_out2020/Time%20series%20of%20Principal%20Component%20Analysis.csv"
  ) %>%
  filter(year == max(year))

list_2020 <- list.2020.full %>%
  filter(Aggregate.PC1 < -1.2) %>%
  mutate(country = country.code.name(iso3c)) %>%
  select(iso3c, country, score = Aggregate.PC1)

list_2022 <-
  setDT(final_results_output)[type != "Rest of the world", .(iso3c,
                                                             country = country.code.name(iso3c),
                                                             score)]

#custom function for the analysis below
cbindPad <- function(...) {
  args <- list(...)
  n <- sapply(args, nrow)
  mx <- max(n)
  pad <- function(x, mx) {
    if (nrow(x) < mx) {
      nms <- colnames(x)
      padTemp <- matrix("", mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x) == 0) {
        return(padTemp)
      } else {
        return(rbind(x, padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args, pad, mx)
  return(do.call(cbind, rs))
}

#countries that were on the 2020 list but are not on the 2022 list
incl_2020 <-
  as.data.frame(matrix(
    setdiff(list_2020$country, list_2022$country),
    dimnames = list(NULL, c("Countries Moving off the List"))
  ))

#countries that were not on the 2020 list but are on the 2022 list
excl_2020 <-
  as.data.frame(matrix(
    setdiff(list_2022$country, list_2020$country),
    dimnames = list(NULL, c("Countries Moving onto the List"))
  ))

changes_from_2020 <- cbindPad(incl_2020, excl_2020)

changes_bt_lists <- list("From 2020 to 2022" = changes_from_2020)

write.xlsx(
  changes_bt_lists,
  paste0(output_folders, "changes/Changes Between Lists.xlsx"),
  overwrite = T
)

# Viz - PCA Analysis ------------------------------------------------------

#extract and visualise latest year (in this case, 2021)
all.pca.latest <- lapply(all.pca, function(i) {
  i[["2021"]]
})

#extract variables from PCA analysis
pca.var.dimension <- lapply(all.pca.latest, function(i) {
  get_pca_var(i)
})

#extract individuals from PCA analysis
pca.ind.dimension <- lapply(all.pca.latest, function(i) {
  get_pca_ind(i)
})

##CONTRIBUTION - BAR PLOTS
#visualise contributions to the first two dimensions (PCAs), using bar plots.
#contributions is a measure of which variables are the most important in explaining variability in a dataset
#the ones correlated with the first two PCAs are the most important in explaining that
#variability.

#calculate contributions in table


all.pca.latest.contrib <-
  lapply(all.pca.latest, function(i)
  {
    res.var <- get_pca_var(i)
    res.var.contrib <- res.var$contrib
    res.var.pcas <- res.var.contrib[, 1:2]
    res.var.pcas.combined <- res.var.pcas %>%
      as.data.frame() %>%
      rownames_to_column()
  })

all.pca.latest.contrib.aggregate <-
  all.pca.latest.contrib$Aggregate %>%
  select(dimension = rowname,
         pca = Dim.1) %>%
  separate(dimension, c("dimension", "pc")) %>%
  group_by(dimension) %>%
  summarise(pca = sum(pca)) %>%
  arrange(-pca)
fwrite(all.pca.latest.contrib.aggregate,
       "./data_out2022/Results/Contributions.csv")


contrib.bar.viz <-
  lapply(all.pca.latest, function(i) {
    fviz_contrib(i,
                 choice = "var",
                 axes = 1:2,
                 ggtheme = theme_classic()) +
      ggtitle(paste("Contributions to PC1 and PC2 in the", i$dimension, "Dimension")) +
      theme(axis.text.x = element_text(size = 8))
  })

lapply(names(contrib.bar.viz),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC1and2/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz[[x]],
           width = 15
         ))

contrib.bar.viz.Dim1 <-
  lapply(all.pca.latest, function(i) {
    fviz_contrib(i,
                 choice = "var",
                 axes = 1,
                 ggtheme = theme_classic()) +
      theme(axis.text.x = element_text(size = 8)) +
      ggtitle(paste("Contributions to PC1 in the", i$dimension, "Dimension"))
  })

lapply(names(contrib.bar.viz.Dim1),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC1/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz.Dim1[[x]],
           width = 15,
           height = 10
         ))

contrib.bar.viz.Dim2 <-
  lapply(all.pca.latest, function(i) {
    fviz_contrib(i, choice = "var",
                 axes = 2) +
      ggtitle(paste("Contributions to PC2 in the", i$dimension, "Dimension")) +
      theme(axis.text.x = element_text(size = 8))
  })

lapply(names(contrib.bar.viz.Dim2),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC2/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz.Dim2[[x]],
           width = 15
         ))

# Aggregate Changes -------------------------------------------------------

agg = lapply(all.pca$Aggregate, function(x) {
  x = data.frame(iso3c = x$iso3c, value = x$x[, 1])
  return(x)
})
agg = bind_rows(agg, .id = "year") %>%
  mutate(year = as.integer(as.character(year)),
         country = country.code.name(iso3c)) %>%
  arrange(-year) %>%
  select(iso3c, country, year, value)

##global fragility - shifts over time
agg_overtime_pop_weighted <- agg %>%
  left_join(population_cleaned %>% mutate(year = as.integer(year)),
            by = c("year", "iso3c")) %>%
  mutate(category = ifelse(
    value < ext_frag_threshold,
    "Extremely fragile",
    ifelse(value < frag_threshold, "Other fragile",
           "Rest of the world")
  )) %>%
  group_by(year) %>%
  mutate(scaled_value = rescale(value, c(100, 0)))

agg_overtime_pop_weighted_all <- agg_overtime_pop_weighted %>%
  group_by(year) %>%
  summarise(value = weighted.mean(scaled_value, population, na.rm = T))

agg_overtime_pop_weighted_disag <- agg_overtime_pop_weighted %>%
  group_by(year, category) %>%
  summarise(value = weighted.mean(scaled_value, population, na.rm = T))

export_agg_overtime <- list("All" = agg_overtime_pop_weighted_all,
                            "Disaggregated" = agg_overtime_pop_weighted_disag)
write.xlsx(
  export_agg_overtime,
  paste0(
    output_folders,
    "changes/Changes in Fragility - shifting definitions.xlsx"
  ),
  overwrite = T
)

##Which countries have been fragile over time
#define thresholds

#create dataframes of countries that have been fragile and extremely fragile each year, using a TRUE/FALSE
overtime <- setDT(agg)[, .(
  iso3c,
  value,
  fragile = value < frag_threshold,
  other_fragile = between(value, ext_frag_threshold, frag_threshold),
  extreme_fragile = value < ext_frag_threshold
), year][year >= 2010, .(year, iso3c = as.character(iso3c), fragile, extreme_fragile)] #subset greater than 2010
overtime$country <- country.code.name(overtime$iso3c)
overtime_fragile <-
  overtime %>% dplyr::select(-extreme_fragile) %>% dplyr::mutate(fragile = as.numeric(fragile)) %>% spread(year, fragile) %>% janitor::adorn_totals()
overtime_extremely_fragile <-
  overtime %>% dplyr::select(-fragile) %>% dplyr::mutate(extreme_fragile = as.numeric(extreme_fragile)) %>%
  spread(year, extreme_fragile) %>%
  janitor::adorn_totals()

#export that dataframe
list_overtime_categories <-
  list("Fragile" = overtime_fragile, "Extremely Fragile" = overtime_extremely_fragile)
library(openxlsx)
write.xlsx(
  list_overtime_categories,
  paste0(
    output_folders,
    "changes/Changes between Categories Over time.xlsx"
  ),
  overwrite = T
)

# # Dimensional and Variable Changes -----------------------------------------------------------------
# ### Dimensional-level Deltas -------------------------------------------------------------
dimensional_changes_disag_all <-
  raw.data %>%
  dplyr::mutate(year = as.integer(year)) %>%
  arrange(-year) %>%
  gather(variablename, value,-c(iso3c, year)) %>%
  dplyr::filter(grepl("PC1", variablename))

deltas_start_year <- 2019
deltas_end_year <- 2021

deltas_all <-
  hpc.change(dimensional_changes_disag_all) %>%
  dplyr::filter(from == deltas_start_year,
                to == deltas_end_year) %>%
  dplyr::arrange(absolute.diff) %>%
  dplyr::mutate(
    change = ifelse(absolute.diff < 0,
                    "Deterioration",
                    "Improvement"),
    country = country.code.name(iso3c)
  ) %>%
  dplyr::left_join(final_results_output[, c("iso3c", "type")], by = "iso3c") %>%
  dplyr::select(iso3c, country, type, everything())

deltas_all_agg <- deltas_all %>%
  filter(variablename == "Aggregate.PC1") %>%
  mutate(country = oecd.country.name(iso3c))

#chart deltas over time
dim_change_all_plot <- dimensional_changes_disag_all %>%
  mutate(variablename = gsub(".PC1", "", variablename)) %>%
  filter(variablename != "Aggregate") %>%
  rename(dimension = variablename) %>%
  left_join(population_cleaned, c("year", "iso3c")) %>%
  group_by(year, dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

dim_change_all_plot_agg <- dim_change_all_plot %>%
  group_by(year, dimension) %>%
  summarise(value = weighted.mean(value, population))

dim_change_all_plot_agg_last <- dim_change_all_plot_agg %>%
  ungroup() %>%
  filter(year == max(year))


#plot deteriorations and improvements across the dimensions of fragility for all fragile contexts
deltas_all_plotting <- deltas_all %>%
  rename(dimension = variablename) %>%
  ungroup() %>%
  mutate(
    dimension = gsub(".PC1", "", dimension),
    country = oecd.country.name(iso3c),
    absolute_values = abs(absolute.diff)
  ) %>%
  select(
    iso3c,
    country,
    dimension,
    absolute.diff,
    absolute_values,
    prop.growth,
    annual.prop.growth
  ) %>%
  filter(dimension != "Aggregate")

deltas_all_frag <- deltas_all_plotting %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c))

deltas_all_extfrag <- deltas_all_plotting %>%
  filter(iso3c %in% unique(ext_frag$iso3c))

deltas_all_extfrag$fill_coll <-
  ifelse(
    deltas_all_extfrag$absolute.diff > 0.5,
    "#2b8a75",
    ifelse(deltas_all_extfrag$absolute.diff < -0.5,
           "#ffd200",
           "#e0f7d2")
  )
deltas_all_extfrag$fill_coll <- factor(deltas_all_extfrag$fill_coll)

deltas_frag_top10_dim <- deltas_all_plotting %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c)) %>%
  group_by(dimension) %>%
  slice_max(absolute_values, n = 10)

#growth top ten
deltas_frag_top10_dim_plot <- deltas_frag_top10_dim %>%
  select(country, dimension, prop.growth) %>%
  mutate(direction = ifelse(prop.growth < 0, "Decline",
                            "Increase"))

#export deteriorations and improvements
##first, create subset of deltas dataset with only the countries that have experienced deteriorations in this time period in the aggregate
deltas_fragile_agg_deterioration <-
  deltas_all %>%
  dplyr::filter(
    variablename == "Aggregate.PC1",
    change == "Deterioration",
    type %in% c("Other fragile",
                "Extremely fragile")
  )
##prepare a subset of all other dimensions, using the previous subsets - you will rbind these, and then split them into a list for ease of export
deltas_fragile_other_deterioration <-
  deltas_all %>%
  dplyr::filter(
    variablename != "Aggregate.PC1",
    iso3c %in% unique(deltas_fragile_agg_deterioration$iso3c)
  )

deltas_fragile_deterioration <-
  rbind(deltas_fragile_agg_deterioration,
        deltas_fragile_other_deterioration) %>%
  arrange(iso3c) %>%
  split(.$iso3c)

write.xlsx(
  deltas_fragile_deterioration,
  paste0(
    output_folders,
    "changes/Deterioration within Each Dimension - by Context.xlsx"
  ),
  overwrite = T
)

##second, create subset of deltas dataset with only the countries that have experienced improvements in this time period in the aggregate
deltas_fragile_agg_improvement <-
  deltas_all %>%
  dplyr::filter(
    variablename == "Aggregate.PC1",
    change == "Improvement",
    type %in% c("Other fragile",
                "Extremely fragile")
  )
##prepare a subset of all other dimensions, using the previous subsets - you will rbind these, and then split them into a list for ease of export
deltas_fragile_other_improvement <-
  deltas_all %>%
  dplyr::filter(
    variablename != "Aggregate.PC1",
    iso3c %in% unique(deltas_fragile_agg_improvement$iso3c)
  )

deltas_fragile_improvement <- rbind(deltas_fragile_agg_improvement,
                                    deltas_fragile_other_improvement) %>%
  arrange(iso3c) %>%
  split(.$iso3c)
write.xlsx(
  deltas_fragile_improvement,
  paste0(
    output_folders,
    "changes/Improvement within Each Dimension - by Context.xlsx"
  ),
  overwrite = T
)

### Variable-level Deltas ----------------------------------------------------------------

variable_changes_disag <- sfr.time.series %>%
  dplyr::filter(year %in% c(2010:2021)) %>%
  dplyr::mutate(
    location = country.code.name(iso3c),
    variablename = gsub("\\(C\\)", "", variablename),
    variablename = gsub("\\(R\\)", "", variablename),
    variablename = trimws(variablename)
  )

variable_changes_disag_subset <- variable_changes_disag %>%
  dplyr::select(year, iso3c, variablename, value)

deltas_byvar_final <- hpc.change(variable_changes_disag_subset) %>%
  dplyr::filter(from == deltas_start_year, to == deltas_end_year) %>%
  arrange(absolute.diff) %>%
  dplyr::select(-c(prop.growth, annual.prop.growth)) %>%
  dplyr::left_join(
    variable_changes_disag %>% distinct(variablename, dimension, doesmoreincreasefragility),
    "variablename"
  )

#need to be careful about the interpretation, because it is based on the directionality of the raw variable
deltas_byvar_bydirectionality <-
  deltas_byvar_final %>%
  split(.$doesmoreincreasefragility)
deltas_byvar_one <-
  deltas_byvar_bydirectionality$`1` %>%
  dplyr::select(-doesmoreincreasefragility) %>%
  dplyr::distinct()
deltas_byvar_zero <-
  deltas_byvar_bydirectionality$`0` %>% dplyr::select(-doesmoreincreasefragility) %>%
  dplyr::distinct()

deltas_byvar_one$change <-
  ifelse(
    deltas_byvar_one$absolute.diff < 0,
    "Improvement",
    ifelse(
      deltas_byvar_one$absolute.diff == 0,
      "No Change",
      "Deterioration"
    )
  )
deltas_byvar_zero$change <-
  ifelse(
    deltas_byvar_zero$absolute.diff < 0,
    "Improvement",
    ifelse(
      deltas_byvar_zero$absolute.diff == 0,
      "No Change",
      "Deterioration"
    )
  )
deltas_by_var_final_exp <-
  rbind(deltas_byvar_one, deltas_byvar_zero)
deltas_list <- list("Dimension" = deltas_all,
                    "Variables" = deltas_by_var_final_exp)
write.xlsx(
  deltas_list,
  paste0(
    output_folders,
    "changes/Changes - All Contexts, by Variable and Dimension.xlsx"
  ),
  overwrite = T
)

# Groupings Analysis - calculating averages by group ------------------------------------------------------

#all contexts first

group_scores_raw <- raw.data %>%
  dplyr::select(iso3c, year, ends_with("PC1")) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(-year) %>%
  dplyr::mutate_at(vars(-iso3c), function(x)
    rescale(x, c(100, 0))) %>%
  dplyr::left_join(
    population_cleaned %>%
      dplyr::filter(year == 2021) %>%
      dplyr::select(iso3c, population),
    "iso3c"
  ) %>%
  dplyr::left_join(iso_bygroup, "iso3c")

group_scores_aggregated_raw <- group_scores_raw %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",-c("iso3c", "group", "population")) %>%
  dplyr::group_by(dimension, group) %>%
  dplyr::summarise(raw_value = weighted.mean(value, population, na.rm = T))

write.xlsx(
  group_scores_aggregated_raw,
  paste0(output_folders, "./Results/Group scores (single year).xlsx"),
  overwrite = T
)


dat <- group_scores_raw %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",
               -c(iso3c, group, population)) %>%
  mutate(
    dimension = gsub(".PC1", "", dimension),
    group = gsub("WB Classes - ", "", group)
  ) %>%
  filter(
    group %in% c(
      "Sub-Saharan Africa",
      "East Asia & Pacific",
      "Europe & Central Asia",
      "South Asia",
      "Middle East & North Africa",
      "Latin America & Caribbean"
    )
  ) %>%
  split(.$dimension)

#overtime
group_overtime_scores_raw_unweighted <- raw.data %>%
  filter(iso3c %in% unique(oda_recipients$iso3c)) %>%
  select(iso3c, year, ends_with("PC1")) %>%
  pivot_longer(names_to = "dimension", values_to = "value", -c(year, iso3c)) %>%
  group_by(year, dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  ungroup() %>%
  dplyr::left_join(iso_bygroup, "iso3c") %>%
  group_by(year, dimension, group) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  split(.$dimension) %>%
  map(
    function(x)
      x %>% pivot_wider(names_from = "year", values_from = "value") %>%
      rowwise() %>%
      mutate(rate_of_change = (`2021` / `2016` - 1) * 100)
  )
write.xlsx(
  group_overtime_scores_raw_unweighted,
  paste0(
    output_folders,
    "./Results/Scaled group scores - overtime (no weighting).xlsx"
  ),
  overwrite = T
)

group_overtime_scores_raw <- raw.data %>%
  select(iso3c, year, ends_with("PC1")) %>%
  pivot_longer(names_to = "dimension", values_to = "value", -c(year, iso3c)) %>%
  group_by(year, dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  ungroup() %>%
  dplyr::left_join(
    population_cleaned %>% mutate(year = as.character(year)) %>%
      dplyr::select(year, iso3c, population),
    c("year", "iso3c")
  ) %>%
  dplyr::left_join(iso_bygroup, "iso3c") %>%
  group_by(year, dimension, group) %>%
  summarise(value = weighted.mean(value, population, na.rm = T)) %>%
  ungroup()

write.xlsx(
  group_overtime_scores_raw,
  paste0(
    output_folders,
    "./Results/Scaled group scores - overtime (weighting).xlsx"
  ),
  overwrite = T
)

group_overtime_scores_raw_FS <- raw.data %>%
  filter(iso3c %in% unique(oecd_fragile_all$iso3c)) %>%
  select(iso3c, year, ends_with("PC1")) %>%
  pivot_longer(names_to = "dimension", values_to = "value", -c(year, iso3c)) %>%
  group_by(year, dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  ungroup() %>%
  dplyr::left_join(
    population_cleaned %>% mutate(year = as.character(year)) %>%
      dplyr::select(year, iso3c, population),
    c("year", "iso3c")
  ) %>%
  dplyr::left_join(iso_bygroup, "iso3c") %>%
  group_by(year, dimension, group) %>%
  summarise(value = weighted.mean(value, population, na.rm = T)) %>%
  ungroup()

write.xlsx(
  group_overtime_scores_raw_FS,
  paste0(
    output_folders,
    "./Results/Scaled group scores - overtime (weighting) FS only.xlsx"
  ),
  overwrite = T
)