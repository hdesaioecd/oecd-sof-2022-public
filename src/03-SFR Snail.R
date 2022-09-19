source("./lib/funcs.R")
library(rio)

#for these biplots, you only need the PCA for the latest year
#the all.pca list is arranged into dimensions (and an aggregate), by year
output_folders <- c("./data_out2022/")

# Number of clusters - set to six
clusters = lapply(all.pca, function(x) {
  temp = last(x)
  temp = cluster(temp, num.clusters = 6)
  temp = temp$labels %>% dplyr::select(iso3c, cluster)
})
clusters = bind_rows(clusters, .id = "dimension")

aggr.pca <- data.frame(
  iso3c = all.pca$Aggregate$`2021`$iso3c,
  fragility.level = all.pca$Aggregate$`2021`$x[, 1]
) %>%
  arrange(fragility.level) %>%
  left_join(clusters, by = "iso3c")

######import qualitative decisions on clusters
fragile.levels <-
  read_excel("./data/additional data/dimensional fragility.xlsx")
aggr.pca <-
  left_join(aggr.pca, fragile.levels, by = c("dimension", "cluster"))

aggr.pca$country <- oecd.country.name(aggr.pca$iso3c, short = F)
aggr.pca$country <- factor(aggr.pca$country,
                           levels = unique(aggr.pca$country),
                           ordered = T)

# Step 8 - Pie Chart ------------------------------------------------------
##DAVID - is there any way to reduce the distance between text and axis?
#create list
aggr.pca.pie <-
  aggr.pca  %>%
  dplyr::select(country, dimension, fragility.level, Fragility) %>%
  filter(fragility.level < frag_threshold,
         dimension != "Aggregate") %>%
  mutate(
    country = gsub("Democratic People’s Republic of Korea",
                   "DPRK", country),
    country = gsub("Democratic Republic of the Congo",
                   "DRC", country),
    country = gsub("Syrian Arab Republic",
                   "Syria", country),
    country = gsub("Central African Republic",
                   "Central African Rep.", country),
    country = gsub("West Bank and Gaza Strip",
                   "West Bank & Gaza", country)
  ) %>%
  mutate(country = factor(country,
                          levels = unique(country),
                          ordered = T))

aggr.pca_adj <- aggr.pca %>%
  filter(fragility.level < frag_threshold)

angles <-
  -90 - 270 / length(unique(aggr.pca.pie$country)) *
  seq_along(aggr.pca.pie$country)
angles <- angles %% 360
pos <- angles > 90
angles[pos] <- angles[pos] - 180
aggr.pca.pie$dimension <-
  factor(aggr.pca.pie$dimension,
         levels = rev(
           c(
             "Economic",
             "Environmental",
             "Human",
             "Political",
             "Security",
             "Societal"
           )
         ),
         ordered = T)


#COLOURS OF COUNTRY NAMES
ybreaks <- levels(aggr.pca.pie$dimension)
aggr.pca.pie$col <- brewer.pal(9, "Blues")[9]
aggr.pca.pie$col[aggr.pca.pie$fragility.level == "Fragile"] <-
  brewer.pal(9, "Blues")[6]
col <- aggr.pca.pie  %>% dplyr::select(country, col) %>% distinct()
p <-
  ggplot(aggr.pca.pie , aes(x = country, y = dimension)) +
  geom_tile(aes(fill = Fragility),
            alpha = 0.6,
            colour = grey(0.6))
upper.lim <- ceiling(10 * nlevels(aggr.pca.pie$country))
cols <- brewer.pal(6, "Reds")

# # the amrgins are top, right, bottom, lef, 
# q = p + theme(axis.text.x = element_text(#angle = 0, 
#                                          hjust =0, 
#                                          size = 10, 
#                                          vjust=-20, 
#                                          margin=margin(0,0,0,0)))


#COLOURS OF DIMENSIONAL SNAIL
p <-
  p + scale_fill_gradientn(
    colours = colorRampPalette(brewer.pal(9, "Blues")[9:4])(5),
    limits = c(1, 5),
    breaks = c(1, 5),
    labels = c("Severe", "Minor")
  )

p <- p + scale_x_discrete(expand = c(0.163, 0))
p <- p + scale_y_discrete(expand = c(0.5, 0))

p <-
  p + theme(axis.text.x = element_text(
    angle = angles,
    vjust = 0.5,
    colour = col$col
  ))
p <- oecd.plot(p)

p <-
  p + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.direction = "horizontal"
  )


p <- p + coord_polar(start = -0.8)
p <- p + xlab("") + ylab("")

p <-
  p + geom_text(
    data = data.frame(x = 0.5, y = ybreaks, label = ybreaks),
    aes(x = x, y = y, label = label),
    inherit.aes = F,
    size = 3
  )

p <-
  p + theme(legend.position = c(0.2, 0.8),
            legend.background = element_rect(fill = rgb(233 / 255, 237 / 255,
                                                        247 /
                                                          255)))

fname = paste0("./graphs/", length(unique(aggr.pca.pie$country)), "-Fragile-Situations.jpg")
ggsave(p,
       filename = fname,
       height = 10,
       width = 20)