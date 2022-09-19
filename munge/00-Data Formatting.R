# Create output folders ---------------------------------------------------

if (!dir.exists("./data_out2022/Data availability"))
{
  dir.create("./data_out2022/Data availability")
}

if (!dir.exists("./data_out2022/Data checks"))
{
  dir.create("./data_out2022/Data checks")
}

if (!dir.exists("./data_out2022/Results"))
{
  dir.create("./data_out2022/Results")
}


# Load libraries and create main list -------------------------------------

#load libraries
source("./lib/load-libraries.R")
#import main log file
sfr.log <-
  read_excel("./data/indicator master list/SFR2022 Indicator Master List.xlsx",
             "raw.data.for.R") %>%
  select(-notes)

#create a main list for the rest of the munging process
raw.data <- list(log = sfr.log)

#check number of risks and coping capacities by dimension on all rows where include = 1
#export to data availability folder as a check
sfr.log.type <- setDT(sfr.log)[include == 1,
                               .N, .(dimension, type)]

sfr.log.type.wider <- sfr.log.type %>%
  tidyr::pivot_wider(names_from = type,
              values_from = N) %>%
  janitor::adorn_totals("col")

fwrite(
  sfr.log.type.wider,
  "./data_out2022/Data availability/Distribution of risks and coping capacities.csv"
)
rmExcept("raw.data")