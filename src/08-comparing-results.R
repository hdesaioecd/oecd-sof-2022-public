library(dplyr)
library(tidyverse)

setwd("./data_out2022/Results/to_check")

orig_df = rio::import("export of final results.xlsx")
fixed_df =  rio::import("export of final results_21 06 22 14 16.xlsx")


#------------------------------------------------------------------------------
# check diferences in other fragile
orig_other_fragile = orig_df %>%
  filter(type %in% c("Other fragile")) %>%
  pull(country)

fixed_other_fragile = fixed_df %>%
  filter(type %in% c("Other fragile")) %>%
  pull(country)

# check for differene in countries 
T1 = setdiff(fixed_other_fragile,orig_other_fragile)
T2  =setdiff(orig_other_fragile,fixed_other_fragile)
T1
T2


#-----------------------------------------------------------------------------
# check for differences in extremely fragile
orig_extremely_fragile = orig_df %>%
  filter(type %in% c("Extremely fragile")) %>%
  pull(country)

fixed_extremely_fragile = fixed_df %>%
  filter(type %in% c("Extremely fragile")) %>%
  pull(country)

T3 = setdiff(fixed_extremely_fragile,orig_extremely_fragile)
T4  =setdiff(orig_extremely_fragile,fixed_extremely_fragile)
T3
T4

#-----------------------------------------------------------------------------
# check for differences in rest of the world
orig_rest= orig_df %>%
  filter(type %in% c("Rest of the world")) %>%
  pull(country)

fixed_test = fixed_df %>%
  filter(type %in% c("Rest of the world")) %>%
  pull(country)

T5 = setdiff(fixed_test,orig_rest)
T6  =setdiff(orig_rest,fixed_test)
T5
T5

#-----------------------------------------------------------------------------


# making a table of the ranking differences

orig_df = rio::import("export of final results.xlsx")
fixed_df =  rio::import("export of final results_21 06 22 14 16.xlsx")

# rename columns for joining
orig_df = orig_df %>% rename(original_score = score,
                             original_type = type) %>%
  mutate(original_rank = rank(original_score))



fixed_df = fixed_df %>% rename(new_score = score,
                               new_type = type)%>%
  mutate(new_rank = rank(new_score))


full_df = left_join(orig_df, fixed_df, by=c("iso3c","country"))

# calculte the difference in rank and the percentage change in score 
full_df = full_df %>% mutate(difference_rank = new_rank - original_rank)
full_df$per_chg_score = ((full_df$new_score - full_df$original_score)/full_df$original_score)*100

# save the output
rio::export(full_df, "full_df_results_comparison.csv")

# Old 
#reduced_df = full_df %>% select(iso3c, country, original_rank, new_rank, difference_rank)
#rio::export(reduced_df, "reduced_df_results_comparison.csv")