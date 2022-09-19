ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <-
  c(
    "ProjectTemplate",
    "plyr",
    "rworldmap",
    "maptools",
    "gridExtra",
    "DataCombine",
    "gdata",
    "readxl",
    "countrycode",
    "testthat",
    "tools",
    "caret",
    "RColorBrewer",
    "FactoMineR",
    "scales",
    "WDI",
    "VIM",
    "tidyverse",
    "stringr",
    "digest",
    "padr",
    "rio",
    "gtsummary",
    "daiR",
    "igraph",
    "data.table",
    "janitor",
    "factoextra",
    "openxlsx",
    "devtools",
    "ggpmisc",
    "httr",
    "wbstats",
    "roll",
    "vtable",
    "zoo",
    "arrow",
    "BAMMtools"
  )
ipak(packages)
