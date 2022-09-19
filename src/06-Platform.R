#script for all updates to the SoF platform

#first,take care of the snail
agg <- final_results_output %>%
  filter(score < frag_threshold) %>%
  arrange(score) %>% 
  select(iso3c,'2021' = score) 
fwrite(agg,"./platform/agg.csv")


# Re-scale from 0 to 100 --------------------------------------------------

rescaled_scores <- final_results_output %>%
  mutate(rescaled_score = rescale(score,c(100,0)))
fwrite(rescaled_scores,"./data_out2022/Results/Rescaled scores.csv")

#calculate each of the dimensional orderings as well, keeping in mind that they should be ordered according to their fragility score within a given cluster
clusters_platform <- clusters_export_for_designer %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",-c(iso3c,country)) %>% 
  mutate(value = gsub("Darkest Shade","1",value),
         value = gsub("Second-darkest shade","2",value),
         value = gsub("Third-darkest shade","3",value),
         value = gsub("Fourth-darkest shade","4",value),
         value = gsub("Lightest shade","5",value))

dim_scores <- raw.data %>% 
  filter(year == 2021) %>% 
  select(iso3c,ends_with(".PC1")) %>% 
  pivot_longer(names_to = "dimension",
               values_to = "frag.score",-iso3c) %>% 
  mutate(dimension = gsub(".PC1","",dimension))

clusters_platform_exp <- clusters_platform %>% 
  left_join(dim_scores,c("iso3c","dimension")) %>% 
  group_by(dimension,value) %>% 
  arrange(frag.score) %>% 
  split(.$dimension) %>% 
  map(function(x) x %>% 
        ungroup() %>% 
        select(iso3c,value,frag.score) %>% 
        mutate(id = row_number(),
               `2021` = paste0(value,".0",id)) %>% 
        select(iso3c,`2021`))
write.xlsx(clusters_platform_exp,"./platform/clusters.xlsx", overwrite = T)

clusters_overall_scores <- clusters_platform %>% 
  split(.$dimension) %>% 
  map(function(x) x %>% 
        select(iso3c,value) %>% 
        rename(`2021` = value))
write.xlsx(clusters_overall_scores,"./platform/clusters (unordered).xlsx", overwrite = T)

#indicator exports - create quantiles after filtering, and be sure to reverse order of values so that 1 = severe, 5 = minor. 
indicators <- sfr.time.series %>%
  filter(year == 2021) %>%
  select(iso3c,year,variablename,dimension,value) %>% 
  split(.$variablename) %>% 
  map(function(x) x %>% 
        mutate(`2021` = ntile(-value,5)) %>% 
        select(iso3c,`2021`,dimension) %>% 
        filter(iso3c %in% unique(aggr.pca_adj$iso3c)))