source("./lib/funcs.R")
# Prep --------------------------------------------------------------------

output_folders <- c("./data_out2022/")
if (!dir.exists("./data_out2022"))
{
  dir.create("./data_out2022")
}
save(list = ls(), file = "./cache/errortesting/errortesting.RData")
load("./cache/errortesting/errortesting.RData")

#create a separate log file
sfr.log <- setDT(raw.data$log)[, .(
  reportname,
  variablename,
  dimension,
  type,
  doesmoreincreasefragility,
  include,
  source
)]

#create temporary raw.data in list so that you can refer to it later if needed
old.raw <- raw.data

#set log in list to null because you don't need it.
raw.data$log <- NULL

#apply this to ensure that each element of the list (which has columns) has the same format
#then, combine all elements of the list into one dataset
raw.data <- lapply(raw.data, function(x) {
  x$iso3c <- as.character(x$iso3c)
  x$variablename <- as.character(x$variablename)
  x$year <- as.integer(as.character(x$year))
  x$value <- as.numeric(as.character(x$value))
  return(x)
}) %>%
  bind_rows()

#create country column
raw.data$country <- raw.data$iso3c

#create iso3c columns
raw.data$iso3c <- country.code.name(raw.data$iso3c)
#produce checks on country code to ensure that iso = country names are paired correctly
warning("Please check the output of the country code conversions, especially PSE/WBG")
country.code.check <-
  raw.data %>% drop_na(iso3c) %>% dplyr::select(country, iso3c) %>% distinct() %>% arrange(iso3c)
fwrite(country.code.check, file = "./data_out2022/Data checks/Country code conversions.csv", row.names = F)

#dplyr::filter data that is relevant for the index (include = 1)
raw.data <- left_join(raw.data,
                      sfr.log,
                      by = "variablename") %>%
  filter(include == 1) %>%
  select(iso3c,
         country,
         dimension,
         type,
         reportname,
         year,
         value,
         doesmoreincreasefragility) %>%
  rename(variablename = reportname)

#check time series
time.span = raw.data %>%
  dplyr::group_by(variablename) %>%
  dplyr::summarise(
    min.year = min(year),
    max.year = max(year),
    timespan = max.year - min.year + 1,
    n = n()
  ) %>%
  dplyr::mutate(expected = timespan * 175,
                delta = expected - n)
time.span.filter <- time.span %>% filter(timespan == 1)
fwrite(time.span.filter,
       "./data_out2022/Data checks/only-one-year-of-data.csv")
fwrite(time.span, "./data_out2022/Data checks/full_range_years.csv")

#test that data has formatted properly #i.e. one data point per country-variable name
sfr.time.series <- raw.data
#calculate most recent year here, but first, filter 2011+ so that older data doesn't appear in dataframe:
raw.data <- raw.data %>%
  dplyr::filter(year >= 2011)

#export a list of every country-year pairing for each indicator
export_countryyear <- raw.data %>%
  group_by(iso3c, variablename) %>%
  filter(year == max(year)) %>%
  select(country, dimension, variablename, year)
write.xlsx(
  export_countryyear,
  "./data_out2022/Data availability/Latest year by country-indicator.xlsx",
  overwrite = T
)
#calculate most recent year:
raw.data <- most.recent(raw.data)

# Data availability -------------------------------------------------------

###### remove countries with less than threshold percentage of data
threshold <- 0.700
#calculate availability by indicator:
availability_byind <- raw.data %>%
  group_by(iso3c, dimension, variablename) %>%
  summarise(n = n()) %>%
  arrange(iso3c, dimension)

#availability matrix:
availability <-
  as.data.frame(table(raw.data$iso3c,
                      raw.data$variablename)) %>%
  dplyr::rename(iso3c = Var1,
                reportname = Var2) %>%
  dplyr::group_by(iso3c)

#countries that are missing in each indicator:
availability_missing <- availability %>%
  filter(Freq == 0) %>%
  left_join(sfr.log %>%
              select(reportname, dimension, include, source),
            "reportname") %>%
  filter(include == 1) %>%
  select(-include)

write.xlsx(
  availability_missing %>%
    split(.$iso3c) %>%
    map(function(x)
      x %>% arrange(dimension)),
  "./data_out2022/Data availability/Missing indicators by country.xlsx",
  overwrite = T
)
#calculate availability by country
availability <- availability %>%
  dplyr::summarise(n = n(), missing = sum(Freq == 0) / n()) %>%
  dplyr::mutate(
    iso3c = as.character(iso3c),
    location = country.code.name(iso3c),
    actual = 1 - missing,
    actual_number = actual * n
  )
availability$location <- country.code.name(availability$iso3c)
availability$actual <- 1 - availability$missing
#export this
fwrite(
  availability %>% arrange(-actual),
  paste0(
    output_folders,
    "./Data availability/Global thresholds of data availability.csv"
  )
)

#data availability checks here, to calculate availability by region, grouping, etc.

##import ODA eligible recipients
ODA_recipients <-
  read_excel("./data/additional data/DAC-CRS-CODES.xlsx", sheet = "Recipient") %>%
  clean_names() %>% remove_empty("cols")
ODA_recipients$iso3c <-
  country.code.name(ODA_recipients$recipient_name_en)
#142 ODA-eligible recipients as per latest
ODA_recipients <-
  ODA_recipients %>% dplyr::filter(complete.cases(iso3c),
                                   dac_income_group != "Part I unallocated") %>%
  dplyr::select(iso3c, dac_income_group, country = recipient_name_en, region, sids)
#SIDS - as this is a grouping of interest:
ODA_sids <-
  setDT(ODA_recipients)[sids == 1, .(iso3c, region = "SIDS")]
#all ODA-eligible recipients
ODA_recipients_all <-
  ODA_recipients[, .(iso3c, region = "ODA eligible")]
##import regional and income group classifications from the World Bank here
# using latest June 2021 groupings
WB_categories <-
  read_excel("./data/additional data/CLASS.xlsx",
             "Groups") %>%
  clean_names() %>%
  dplyr::select(iso3c = country_code, region = group_name) %>%
  rbind(ODA_sids) %>%
  rbind(ODA_recipients_all)

#import and join population, using MEDIUM VARIANT estimates of 2021 (as the latest year of data)
pop_path <-
  "./data/population data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"

pop_bycountry <-
  read_excel(pop_path, sheet = "MEDIUM VARIANT", skip = 16) %>%
  population_func() %>%
  filter(year == 2021) %>%
  select(-year)

availability_wclass <- availability %>%
  dplyr::filter(1 - missing >= threshold) %>%
  ungroup() %>%
  left_join(WB_categories, by = "iso3c") %>%
  left_join(pop_bycountry, by = "iso3c")

###by region
WB_categories_byregion <-
  WB_categories %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(count_total = n())

WB_categories_byregion_pop <-
  WB_categories %>%
  left_join(pop_bycountry, by = "iso3c") %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(pop_all = sum(population, na.rm = T) / 1000000)

availability_byregion <-
  availability_wclass %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(count_coverage = n()) %>%
  left_join(WB_categories_byregion, by = "region") %>%
  dplyr::mutate(proportion = count_coverage / count_total * 100)

availability_byregion_pop <-
  availability_wclass %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(pop_coverage = sum(population, na.rm = T) / 1000000) %>%
  left_join(WB_categories_byregion_pop, by = "region") %>%
  dplyr::mutate(proportion = pop_coverage / pop_all * 100) %>%
  drop_na()

#total population covered by framework analysis:
global_coverage <- availability %>%
  dplyr::filter(actual >= .700) %>%
  left_join(pop_bycountry, "iso3c") %>%
  dplyr::summarise(value = sum(population, na.rm = T) / 1000000)

availability_actual <- availability %>%
  filter(actual >= .700)
#plots on coverage statistics
cov_total <- WB_categories %>%
  group_by(region) %>%
  summarise(total = n())
cov_subset <- WB_categories %>%
  filter(iso3c %in% unique(availability_actual$iso3c)) %>%
  group_by(region) %>%
  summarise(subset = n()) %>%
  left_join(cov_total, "region") %>%
  mutate(prop = subset / total * 100)
income_subset <- cov_subset %>%
  filter(region %in% c(
    "Low income",
    "Lower middle income",
    "Upper middle income",
    "High income"
  ))

region_subset <- cov_subset %>%
  filter(
    region %in% c(
      "East Asia & Pacific",
      "Europe & Central Asia",
      "Latin America & Caribbean",
      "Middle East & North Africa",
      "South Asia",
      "Sub-Saharan Africa"
    )
  )

ggplot(data = income_subset,
       mapping = aes(x = reorder(region, -prop),
                     y = prop)) +
  geom_col(fill = "#0B6B66") +
  theme_classic() +
  labs(x = element_blank(),
       y = element_blank()) +
  geom_text(aes(label = paste0(round(prop), "%")),
            vjust = -0.5)
ggsave("./graphs/coverage/share of countries - income.jpg")

ggplot(data = region_subset,
       mapping = aes(x = reorder(region, -prop),
                     y = prop)) +
  geom_col(fill = "#0B6B66") +
  theme_classic() +
  labs(x = element_blank(),
       y = element_blank()) +
  geom_text(aes(label = paste0(round(prop), "%")),
            vjust = -0.5)
ggsave("./graphs/coverage/share of countries - region.jpg",
       width = 10)

###export all of this analysis
list_coverage <- list("Count" = availability_byregion,
                      "Population" = availability_byregion_pop,
                      "Global" = global_coverage)
openxlsx::write.xlsx(
  list_coverage,
  paste0(output_folders,
         "./Data availability/Region and IG Shares.xlsx"),
  overwrite = T
)

# Back to data cleaning: --------------------------------------------------

#remove countries here - final list of countries (should be 176 countries by latest count):
availability <-
  availability %>%
  dplyr::filter(1 - missing >= threshold) %>% ungroup()

warning("Filter here for available countries according to data threshold")
raw.data <-
  raw.data %>%
  dplyr::filter(iso3c %in% as.character(unique(availability$iso3c)))

#output the length of unique countries in console as due diligence check:
length(unique(raw.data$iso3c))

data.matrix.byindicator <-
  raw.data %>%
  dplyr::group_by(variablename, dimension) %>%
  dplyr::summarise(
    min.year = min(year),
    max.year = max(year),
    num.countries = length(unique(iso3c))
  )

tot.num.countries <- data.matrix.byindicator %>%
  ungroup() %>%
  filter(num.countries == max(num.countries)) %>%
  distinct(num.countries) %>%
  pull(num.countries)

#create an export of data availability by indicator, highlighting instances where
#an indicator had too few observations (less than 80% of the max, in this case 141 or less)
write.xlsx(
  data.matrix.byindicator %>% split(.$dimension) %>% map(
    function(x)
      x %>%
      select(-dimension) %>%
      arrange(-num.countries) %>%
      ungroup() %>%
      mutate(
        too_little = ifelse(num.countries / tot.num.countries * 100 <= 80,
                            "TOO FEW OBS", "")
      )
  ),
  "./data_out2022/Data availability/Availability by indicator.xlsx",
  overwrite = T
)

#raw data matrix of values, with years as columns

data.matrix <- raw.data %>%
  dplyr::select(iso3c, country, variablename, dimension, year, value) %>%
  distinct() %>%
  spread(year, value) %>%
  split(.$dimension) %>%
  map(function(x)
    x %>%
      select(-c(dimension,country)) %>%
      mutate(country = country.code.name(iso3c)) %>%
      arrange(variablename))

# invert indicators to be in the same direction - this is an important step to ensure that
# risks/coping capacities are facing the same way
pos <- raw.data$doesmoreincreasefragility == 0
raw.data$value[pos] <- -raw.data$value[pos]

#create a matrix of country rankings by indicator (only for quick spot-checks, not for analysis)
data.matrix.rankings <- raw.data %>%
  select(-c(type, doesmoreincreasefragility)) %>%
  group_by(dimension, variablename) %>%
  mutate(rank = rank(-value, ties.method = "first")) %>%
  select(-c(year, value)) %>%
  split(.$dimension) %>%
  map(
    function(x)
      x %>%
      ungroup(dimension) %>%
      select(-c(dimension,country)) %>%
      mutate(country = country.code.name(iso3c)) %>%
      arrange(iso3c) %>%
      pivot_wider(names_from = variablename,
                  values_from = rank)
  )

write.xlsx(
  data.matrix,
  paste0(
    "./data_out2022/Results/raw_dataset/Raw dataset - values_",
    format(Sys.time(), "%d %m %y %H %M"),
    "_.xlsx"
  )
)

write.xlsx(
  data.matrix.rankings,
  paste0(
    "./data_out2022/Results/raw_dataset/Raw dataset - rankings_",
    format(Sys.time(), "%d %m %y %H %M"),
    "_.xlsx"
  )
)

#create a dataframe of indicators, before imputations
pre_imputed <- raw.data %>%
  dplyr::group_by(variablename) %>%
  dplyr::summarise(n = n())

warning("Imputation happens here")
warning("At this stage, please also make sure that indicators are facing in the same direction")
# set use.precomputed = FALSE so that imputed values are re-calculated with each change in the original indicators
raw.data <- impute(raw.data, use.precomputed = F)

raw.data$country <- oecd.country.name(raw.data$iso3c, short = T)
raw.data$country <- iconv(raw.data$country, "latin1", "UTF-8")

#do the above for the time series - the latest year is what you will use for the PCA
#reverse the value of the variables where doesmoreincreasefragility = 0, same as before
pos <- sfr.time.series$doesmoreincreasefragility == 0
sfr.time.series$value[pos] <- -sfr.time.series$value[pos]

#only include countries that meet the threshold in time series (same as done above)
sfr.time.series = sfr.time.series %>%
  dplyr::filter(iso3c %in% unique(raw.data$iso3c))

#output length of unique iso3c values as check:
length(unique(sfr.time.series$iso3c))

#create temporary dataset - this code is pedantic, but remove value column and change the name of the
#imputed column to reflect value. the only difference between temp and raw.data is that column.
temp = raw.data %>% dplyr::select(-value) %>%
  dplyr::rename(value = imputed)
temp = temp[, names(sfr.time.series)]
temp_counted <- temp %>% group_by(variablename) %>%
  dplyr::summarise(n = n())

#calculate difference between actual and imputed to understand in which indicators there were imputed values
export_imputation <- pre_imputed %>%
  dplyr::left_join(temp_counted, "variablename") %>%
  dplyr::rename(actual = n.x,
                total = n.y) %>%
  dplyr::mutate(difference = total - actual) %>%
  dplyr::arrange(-difference)
fwrite(
  export_imputation,
  paste0(
    output_folders,
    "./Data availability/Imputed values by indicator.csv"
  )
)
#see note below
sfr.time.series <-
  rbind(sfr.time.series,
        temp)  %>%
  dplyr::distinct() #use distinct values, thereby
#removing possible duplication on the rbind between sfr.time.series and temp
#however, this will not remove all duplicates due to sig figs, this is why you need to use mean below

sfr.time.series <-
  setDT(sfr.time.series)[,
                         .(value = mean(value, na.rm = T)),
                         .(iso3c, year, variablename)]

#NOTE - you can adjust the "year" filter here, but removing it does not affect the final results
#since the results are based on the temp file above. setting a year filter will help you
#to account for data limitations in earlier years in time series
sfr.time.series <-
  interpolate(sfr.time.series %>% dplyr::select(iso3c, year, variablename, value)) %>%
  dplyr::rename(imputed = yhat, reportname = variablename) %>%
  left_join(sfr.log, by = "reportname") %>%
  dplyr::filter(include == 1) %>%
  dplyr::select(-variablename) %>%
  dplyr::rename(variablename = reportname) %>%
  dplyr::mutate(value = imputed)

#adjust categorical variables to be rounded to the nearest whole number - do this for the raw.data dataset
cat_vars <- c("Restrictions on political rights",
              "Media freedoms",
              "Perception of corruption")


sfr.time.series <- 
  sfr.time.series %>%
  dplyr::mutate(imputed = ifelse(variablename %in% cat_vars,
                                 round(imputed),
                                 imputed),
                value = ifelse(value  %in% cat_vars,
                               round(imputed),
                               imputed))

raw.data <- 
  raw.data %>%
  dplyr::mutate(imputed = ifelse(variablename %in% cat_vars,
                                 round(imputed),
                                 imputed),
                value = ifelse(value  %in% cat_vars,
                               round(imputed),
                               imputed))

sfr.time.series$imputed = round(sfr.time.series$imputed, 5)
sfr.time.series$value <- round(sfr.time.series$value, 5)
raw.data$imputed = round(raw.data$imputed, 5)
x = most.recent(sfr.time.series) %>% select(-year) %>% arrange(iso3c, variablename) %>%
  dplyr::select(iso3c, variablename, imputed)
y = raw.data %>% select(-year) %>% arrange(iso3c, variablename) %>%
  dplyr::select(iso3c, variablename, imputed)
#test time series matches most recent year
test <- identical(x, y)
expect_that(test, equals(TRUE))

pos <- sfr.time.series$type == "Coping"
sfr.time.series$variablename[pos] <-
  paste(sfr.time.series$variablename[pos], " (C)", sep = "")
sfr.time.series$variablename[!pos] <-
  paste(sfr.time.series$variablename[!pos], " (R)", sep = "")

df.check <- sfr.time.series %>% 
  dplyr::filter(year == max(year)) %>%
  dplyr::group_by(variablename,dimension) %>%
  dplyr::summarise(
    worst = iso3c[which(value == min(value))[1]],
    worst.value = min(value),
    best = iso3c[which(value == max(value))[1]],
    best.value = max(value)
  ) %>%
  dplyr::mutate(best = country.code.name(best),
                worst = country.code.name(worst))
  
fwrite(df.check,
       paste0(output_folders, "./Data checks/Data Check - Directionality.csv"))


# Data availability tests with previous years --------------------------------------------------------------------
available.data.2022 <- availability_byind

sfr.log.2018 <-
  read_excel("./data/indicator master list/SFR2017 Indicator Master List.xlsx",
             sheet = "raw.data.for.R") %>%
  fill(dimension) %>%
  filter(include == 1)

sfr.log.2018.bydim <- sfr.log.2018 %>%
  group_by(dimension) %>%
  summarise(sfr.2018 = n())

sfr.log.2020 <-
  read_excel("./data/indicator master list/SFR2020 Indicator Master List.xlsx",
             sheet = "raw.data.for.R") %>%
  fill(dimension) %>%
  filter(include == 1)

sfr.log.2020.bydim <- sfr.log.2020 %>%
  group_by(dimension) %>%
  summarise(sfr.2020 = n())

sfr.log.bydim <- sfr.log %>%
  filter(include == 1) %>%
  group_by(dimension) %>%
  summarise(total = n())

sfr.log.bydim.overtime <- sfr.log.bydim %>%
  rename(sfr.2022 = total) %>%
  left_join(sfr.log.2020.bydim, "dimension") %>%
  left_join(sfr.log.2018.bydim, "dimension") %>%
  adorn_totals()

sfr.log.indicators <-
  list(
    sfr.2018 = sfr.log.2018 %>% select(dimension, reportname) %>% arrange(dimension),
    sfr.2020 = sfr.log.2020 %>% select(dimension, reportname) %>% arrange(dimension),
    sfr.2022 = sfr.log %>% filter(include == 1) %>% select(dimension, reportname) %>% arrange(dimension)
  )
write.xlsx(
  sfr.log.indicators,
  "./data_out2022/Data availability/SFR indicators comparisons.xlsx",
  overwrite = T
)

available.data.2022.bydim <- available.data.2022 %>%
  group_by(iso3c, dimension) %>%
  summarise(available = n()) %>%
  left_join(sfr.log.bydim, "dimension") %>%
  mutate(prop_available = available / total * 100)

available.data.2022.byind <- available.data.2022 %>%
  group_by(dimension, variablename) %>%
  summarise(available = n()) %>%
  arrange(-available)

available.data <-
  list("Dimension-level" = available.data.2022.bydim,
       "Indicator-level" = available.data.2022.byind)
write.xlsx(
  available.data,
  "./data_out2022/Data availability/Availability by dimension and indicator.xlsx",
  overwrite = T
)
write.xlsx(
  available.data.2022.bydim %>% split(.$iso3c),
  "./data_out2022/Data availability/Availability by country.xlsx",
  overwrite = T
)

# Export indicator summaries ----------------------------------------------
indicator_summ_raw <- raw.data %>%
  select(iso3c, dimension, variablename, value) %>%
  distinct() %>%
  drop_na(value) %>%
  pivot_wider(names_from = c(dimension, variablename),
              values_from = value)

sumtable(
  indicator_summ_raw,
  out = "csv",
  file = paste0(output_folders, "Data checks/Summary statistics.csv")
)

rmExcept(c("raw.data", "sfr.time.series"))