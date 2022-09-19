library(ProjectTemplate)
rm(list = ls())
source("./lib/load-libraries.R")
output_folders <- c("./data_out2022/")
lapply(output_folders, function(x)
  file.remove(list.files(x, full.names = T)))
reload.project(override.config = list(munging = T))
cache("sfr.time.series")
load("./cache/sfr.time.series.RData")
set.seed(12345)
source("./lib/funcs.R")


# Create folders ----------------------------------------------------------

if (!dir.exists("./data_out2022/clusters"))
{
  dir.create("./data_out2022/clusters")
}

if (!dir.exists("./data_out2022/changes"))
{
  dir.create("./data_out2022/changes")
}

if (!dir.exists("./data_out2022/Results/final_results"))
{
  dir.create("./data_out2022/Results/final_results")
}

# ##### Step 1 #### Set parameters for calculations
# # 1. The fragile clusters
# # 2. the number of clusters in each dimension
# # 3. whether to drop highly correlated indicators in each dimension
# # 4. The cluster method, ward.d2 selected as the simplest to explain

drop.indicators.based.on.correlations <- T
all.drops <- NULL
round.numbers <- T

#set thresholds for fragile and extremely fragile
frag_threshold <- -1.20
ext_frag_threshold <- -2.85

####Additional functions

reorient.pca = function(pca) {
  # switch direction for ease of reading
  somalia <- which(pca$iso3c == "SOM")
  iceland <- which(pca$iso3c == "ISL")
  if (length(iceland) > 0 & length(somalia) > 0) {
    if (pca$x[somalia, 1] > pca$x[iceland, 1]) {
      pca$rotation[, 1] <- -pca$rotation[, 1]
      pca$x[, 1] <- -pca$x[, 1]
    }
    if (pca$x[somalia, 2] > pca$x[iceland, 2]) {
      pca$rotation[, 2] <- -pca$rotation[, 2]
      pca$x[, 2] <- -pca$x[, 2]
    }
  }
  return(pca)
}

my.pca = function(temp, dim, yr) {
  pca <- prcomp(temp[,-1], center = TRUE, scale. = TRUE)
  pca$iso3c = temp[, 1]
  pca$data = temp[, -1]
  pca$dimension = dim
  pca$year = yr
  pca = reorient.pca(pca)
  return(pca)
}

my.predict = function(pca, temp, dim, yr) {
  pca$x <- predict(pca, temp[,-1])
  pca$iso3c = temp[, 1]
  pca$data = temp[, -1]
  pca$dimension = dim
  pca$year = yr
  reorient.pca(pca)
  return(pca)
}

double.split = function(df, factor1, factor2) {
  df = as.data.frame(df)
  df = split(df, factor(df[, factor1]))
  df = lapply(df, function(x) {
    split(x, factor(x[, factor2]))
  })
  return(df)
}


################### Step 2 #### Use latest year to calculate PCA models
dimensional.pca.models <-
  sapply(sort(unique(sfr.time.series$dimension)), function(i) {
    # take dimension subset of raw.data
    temp <-
      sfr.time.series %>%
      dplyr::filter(dimension == i, year == max(year))
    temp <- temp %>% dplyr::select(iso3c, variablename, imputed) %>%
      dplyr::arrange(iso3c, variablename)
    temp <- temp %>% distinct() %>% spread(variablename, imputed)
    drops <- findCorrelation(cor(temp[, -1]))
    if (length(drops) > 0 & drop.indicators.based.on.correlations) {
      all.drops <<- 
        rbind(all.drops, data.frame(dimension = i, indicators = names(temp)[drops + 1]))
      data.frame(dimension = i, indicators = names(temp)[drops + 1])
      temp <- temp %>% dplyr::select(-(drops + 1))
    }
    # calculate a pca
    pca <- my.pca(temp, dim = i, yr = max(sfr.time.series$year))
    return(pca)
  }, USE.NAMES = T, simplify = F)

sfr.time.series = sfr.time.series %>%
  dplyr::filter(!(
    paste(variablename, dimension) %in%
      paste(all.drops$indicators, all.drops$dimension)
  ))

dim.pca = double.split(sfr.time.series, "dimension", "year")
for (i in names(dim.pca)) {
  df = dim.pca[[i]]
  for (j in names(df)) {
    temp = df[[j]]
    temp <- temp %>% dplyr::select(iso3c, variablename, imputed) %>%
      dplyr::arrange(iso3c, variablename)
    temp <- temp %>% distinct() %>% spread(variablename, imputed)
    pca = dimensional.pca.models[[i]]
    pca <- my.predict(pca, temp, i, j)
    dim.pca[[i]][[j]] = pca
  }
  #make sure PCA yields the same results for the most recent year
  test = identical(dimensional.pca.models[[i]]$data, dim.pca[[i]][[j]]$data)
  expect_that(test, equals(TRUE))
  test = identical(dimensional.pca.models[[i]]$x, dim.pca[[i]][[j]]$x)
  expect_that(test, equals(TRUE))
}

############################# Aggregate models

agg <- NULL
aggregate.label = "Aggregate"
labels = c(".PC1", ".PC2")
for (i in names(dimensional.pca.models)) {
  temp <- as.data.frame(dimensional.pca.models[[i]]$x[, 1:2])
  names(temp) = paste0(i, labels)
  agg = dplyr::bind_cols(agg, temp)
}
agg = data.frame(iso3c = dimensional.pca.models[[i]]$iso3c, agg)
aggregate.pca.model = my.pca(agg, aggregate.label, max(sfr.time.series$year))

agg.pca <- dim.pca[[1]]
for (j in names(agg.pca)) {
  agg = NULL
  for (i in names(dim.pca)) {
    temp = as.data.frame(dim.pca[[i]][[j]]$x[, 1:2])
    names(temp) = paste0(i, labels)
    agg = bind_cols(agg, temp)
  }
  agg.pca[[j]] = aggregate.pca.model
  agg.pca[[j]] = my.predict(agg.pca[[j]],
                            data.frame(iso3c = agg.pca[[j]]$iso3c, agg),
                            aggregate.label,
                            j)
}

test = identical(aggregate.pca.model$data, agg.pca[[j]]$data)
expect_that(test, equals(TRUE))
test = identical(aggregate.pca.model[[i]]$x, agg.pca[[i]][[j]]$x)
expect_that(test, equals(TRUE))
dim.pca[[aggregate.label]] = agg.pca
all.pca = dim.pca


raw.data = lapply(as.character(2011:2021), function(i) {
  tmp = data.frame(
    iso3c = all.pca$Aggregate[[i]]$iso3c,
    year = i,
    all.pca$Aggregate[[i]]$data,
    Aggregate.PC1 = all.pca$Aggregate[[i]]$x[, 1],
    Aggregate.PC2 = all.pca$Aggregate[[i]]$x[, 2]
  )
  return(tmp)
})
raw.data = bind_rows(raw.data)

final_results_scores <- raw.data %>% filter(year == max(year)) %>%
  mutate(country = country.code.name(iso3c)) %>%
  select(iso3c, country, ends_with(".PC1")) %>%
  arrange(Aggregate.PC1) %>%
  mutate(type = ifelse(
    Aggregate.PC1 < ext_frag_threshold,
    "Extremely fragile",
    ifelse(
      Aggregate.PC1 < frag_threshold,
      "Other fragile",
      "Rest of the world"
    )
  ))

final_results_output <- final_results_scores %>%
  select(iso3c, country, score = Aggregate.PC1, type)

final_results_rankings <- final_results_scores %>%
  mutate(across(where(is.numeric),
                function(x)
                  rank(x)))

final.results.2020 <-
  fread("./data/2020 sfr model data/Time series of PCA 2020.csv")
final.results.2020_clean <- final.results.2020 %>%
  filter(year == max(year)) %>%
  select(iso3c, ends_with(".PC1")) %>%
  mutate(across(where(is.numeric), function(x)
    rank(x))) %>%
  pivot_longer(names_to = 'dimension',
               values_to = 'x2020', -iso3c)

final_results_rankings.long <- final_results_rankings %>%
  select(-c(country, type)) %>%
  pivot_longer(names_to = "dimension",
               values_to = "x2021", -iso3c) %>%
  filter(dimension != "Human.PC1") %>%
  left_join(final.results.2020_clean, c("iso3c", "dimension")) %>%
  mutate(difference = abs(x2021 - x2020),
         country = country.code.name(iso3c)) %>%
  select(iso3c, country, everything())

final_results_timeseries <-
  raw.data %>%
  mutate(country = country.code.name(iso3c)) %>%
  select(iso3c, country, year, everything()) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(desc(year), Aggregate.PC1)

final_results_list <- list(
  "Fragile contexts" = final_results_output,
  "Scores" = final_results_scores,
  "Rankings" = final_results_rankings,
  "Rankings - Comparison" = final_results_rankings.long,
  "Time series" = final_results_timeseries
)

output_folders <- c("./data_out2022/")
write.xlsx(
  final_results_list,
  paste0(
    output_folders,
    "/Results/final_results/export of final results_",
    format(Sys.time(), "%d %m %y %H %M"),
    ".xlsx"
  ),
  overwrite = T
)
rmExcept(
  c(
    "all.pca",
    "raw.data",
    "final_results_output",
    "frag_threshold",
    "ext_frag_threshold",
    "sfr.time.series",
    "final_results_timeseries"
  )
)