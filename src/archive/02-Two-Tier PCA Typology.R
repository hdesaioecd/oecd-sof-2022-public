#### States of Fragility Report 2016 - OECD 
# Written by David Hammond Institute for Economics and Peace 28 May 2016 
# This script: (1) creates a typology of 
# fragility by doing a PCA on the principal components of each dimension
####

source("./lib/funcs.R")
################### Step 1 ######################################### create base data from results

# combine all principal components
all.prcomps <- bind_rows(all.results, .id = "dimension")
all.prcomps <- all.prcomps %>% dplyr::select(-c(cut, labels, value)) %>% gather(variablename, value, -c(dimension, 
    iso3c)) %>% mutate(variablename = paste(dimension, variablename), value = value) %>% dplyr::select(-dimension) %>% 
    spread(variablename, value)  #if I ever want to rename arrow labels do it here
# set the number of clusters to group
num.clusters <- 10

################### Step 2 ######################################### Do a pca of the dimension principal components

pca <- prcomp(all.prcomps[, -1], center = T, scale. = T)
x <- PCA(all.prcomps[, -1], graph = F)
aggr.pca <- data.frame(iso3c = all.prcomps$iso3c, pca$x[, 1:2])
clusters <- hclust(dist(aggr.pca[, -1], method = "euclidean"), method = cluster.method)
clusters.cut <- cutree(clusters, num.clusters)
# calculate centroids for the visualisation
centroids <- as.data.frame(apply(aggr.pca[, -1], 2, function(x) tapply(x, clusters.cut, mean)))
centroids$dist <- with(centroids, sqrt(PC1^2 + PC2^2))
centroids$omega <- atan(centroids$PC2/centroids$PC1)
centroids$cut <- LETTERS[1:nrow(centroids)]
centroids$rank <- rank(centroids$PC1)
centroids$labels <- LETTERS[centroids$rank]
aggr.pca$cut <- LETTERS[clusters.cut]
aggr.pca <- left_join(aggr.pca, select(centroids, cut, labels))
aggr.pca <- aggr.pca %>% dplyr::select(-cut)
# create bi-plot
fragile.clust <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
p <- ggbiplot.sfr(pca, obs.scale = 1, var.scale = 1, ellipse = F, circle = F, labels = aggr.pca$iso3c, 
    groups = aggr.pca$labels, var.axes = T)
rects <- data.frame(xstart = c(-Inf, -2.5, -1.2, 0, 1.2, 2.5), xend = c(-2.5, -1.2, 0, 1.2, 2.5, Inf), 
    col = colorRampPalette(brewer.pal(9, "Blues")[9:2])(6))

p <- p + xlim(c(-6, 6))
p <- p + theme(legend.position = "none")
p <- p + scale_fill_gradientn("Fragility Level", colours = colorRampPalette(brewer.pal(9, "Blues")[9:2])(6), 
    limits = c(1, 6), breaks = c(1, 6), labels = c("High", "Low"))
p <- oecd.biplot(p, coloured.clusters = fragile.clust, n = num.clusters)
p <- p + geom_hline(yintercept = 0, col = muted("darkblue"))
p <- p + geom_vline(xintercept = 0, col = muted("darkblue"))
p <- p + annotate("text", x = -5, y = -4, label = "Predominant Fragility:\nPolitical, Societal\nand/or Security", 
    size = 4)
p <- p + geom_segment(aes(x = -5, xend = -6, y = -0.2, yend = -0.2), arrow = arrow(length = unit(0.1, 
    "cm")))
p <- p + geom_segment(aes(x = 5, xend = 6, y = -0.2, yend = -0.2), arrow = arrow(length = unit(0.1, "cm")))
p <- p + annotate("text", x = -5, y = 2.5, label = "Predominant Fragility:\nEconomic", size = 4)
p <- p + annotate("text", x = -5.5, y = 0.2, label = "Extreme Fragility", size = 4)
p <- p + annotate("text", x = 5.5, y = 0.2, label = "Low Fragility", size = 4)
p <- p + geom_vline(xintercept = -2.5, colour = "red", linetype = "longdash")
p <- p + geom_vline(xintercept = -1.2, color = "red", linetype = "longdash")
p <- p + geom_segment(aes(x = -5, xend = -5, y = -4.5, yend = -5), arrow = arrow(length = unit(0.1, "cm")))
p <- p + geom_segment(aes(x = -5, xend = -5, y = 3, yend = 3.5), arrow = arrow(length = unit(0.1, "cm")))

####will need to decide
p <- p + annotate("text", x = -2.6, y = -5, label = "Extremely Fragile Cutoff", size = 3, angle = 90)
p <- p + annotate("text", x = -1.2, y = -5, label = "Fragile Cutoff", size = 3, angle = 90)

p <- p + xlab("First Principal Component of Fragility") + ylab("Second Principal Component of Fragility")

ggsave(p, filename = paste("./graphs/Fig A8 aggregate PCA.pdf", sep = ""), height = 9, width = 10)

################### Step 2 ######################################### Do a pca of the dimension principal components

aggr.pca <- data.frame(all.prcomps, pca$x[, 1])
###Maybe we want to take all PC1 and not PC2?
aggr.pca <- aggr.pca %>% dplyr::rename(fragility = pca.x...1.) %>% dplyr::select(-contains("PC1"))
aggr.pca <- aggr.pca %>% arrange((fragility))
aggr.pca$fragility.level <- "Rest of the World"
pos <- aggr.pca$fragility <= -1.2 #Take until Malawi
#pos <- as.numeric(rownames(aggr.pca)) < nrow(aggr.pca)/3  #Take 1/3
aggr.pca$fragility.level[pos] <- "Fragile"
pos <- aggr.pca$fragility <= -2.5
aggr.pca$fragility.level[pos] <- "Extreme Fragility"
aggr.pca$country <- oecd.country.name(aggr.pca$iso3c, short = T)
write.csv(aggr.pca, "./data_out2017/two-tier PCA - PC1 as Fragility Score.csv", row.names = F)
#write.csv(aggr.pca, "./data_out2017/Fragility Score Country names.csv", row.names = F)
final <- aggr.pca %>% dplyr::select(iso3c, fragility.level) %>% dplyr::filter(fragility.level != "Rest of the World")
two.lists <- data.frame(all.prcomps$iso3c, pca$x[, 1:2])
two.lists <- two.lists %>% arrange((PC1))
two.lists$fragility.level <- "Rest of the World"
#pos <- as.numeric(rownames(two.lists)) < nrow(two.lists)/3  #Take 1/3
pos <- two.lists$PC1 <= -1.2  #Take until Malawi
two.lists$fragility.level[pos] <- "Fragile"
pos <- two.lists$PC1 <= -2.5
two.lists$fragility.level[pos] <- "Extreme Fragility"
two.lists <- two.lists %>% dplyr::filter(fragility.level != "Rest of the World")
two.lists$above <- findInterval(two.lists$PC2, c(-10, -0.5, 0.5, 10))
two.lists$country <- oecd.country.name(two.lists$all.prcomps.iso3c, short = T)
two.lists <- split(two.lists, factor(two.lists$above))

################### Step 8 ######################################### Pie Chart

temp <- left_join(final, select(fragility, iso3c, country, fragility, dimension))
temp <- temp %>% dplyr::select(country, dimension, fragility.level, fragility)
angles <- -90 - 270/length(unique(temp$country)) * seq_along(temp$country)
angles <- angles%%360
pos <- angles > 90
angles[pos] <- angles[pos] - 180
temp$dimension <- factor(temp$dimension, levels = rev(c("Political", "Societal", "Economic", "Environmental", 
    "Security")), ordered = T)
ybreaks <- levels(temp$dimension)
temp$col <- brewer.pal(9, "Blues")[9]
temp$col[temp$fragility.level == "Fragile"] <- brewer.pal(9, "Blues")[6]
col <- temp %>% dplyr::select(country, col) %>% distinct()
security.cluster <- fragility %>% dplyr::filter(dimension == "Security") %>% dplyr::select(country, cluster)
temp <- left_join(temp, security.cluster)
temp$asterisk <- ""
battledeaths <- raw.data %>% dplyr::filter(variablename == "Battle Related Deaths Per Capita (log)", value > 
    0)
pos <- country.code.name(temp$country) %in% battledeaths$iso3c
temp$country <- factor(temp$country, levels = unique(temp$country), ordered = T)
p <- ggplot(temp, aes(x = country, y = dimension)) + geom_tile(aes(fill = fragility), alpha = 0.6, colour = grey(1))  #geom_bar(stat = 'identity') 
upper.lim <- ceiling(0.3 * nlevels(temp$country))
cols <- brewer.pal(9, "Reds")
p <- p + scale_fill_gradientn(colours = colorRampPalette(brewer.pal(9, "Blues")[9:4])(6), limits = c(1, 
    7), breaks = c(1, 6), labels = c("High", "Med"))

p <- p + scale_x_discrete(expand = c(0.163, 0))
p <- p + scale_y_discrete(expand = c(0.5, 0))

p <- p + theme(axis.text.x = element_text(angle = angles, vjust = 2, colour = col$col))
p <- oecd.plot(p)

p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), legend.direction = "horizontal")
p <- p + coord_polar(start = -0.8)
p <- p + xlab("") + ylab("")

p <- p + geom_text(data = data.frame(x = 0, y = ybreaks, label = ybreaks), aes(x = x, y = y, label = label), 
    inherit.aes = F, size = 4)
p <- p + theme(legend.position = c(0.25, 0.6), legend.background = element_rect(fill = rgb(233/255, 237/255, 
    247/255)))

ggsave(p, filename = "./graphs/Fig 3-1 58 Fragile Situations.pdf", height = 8, width = 10)
aggregate.pca <- PCA(all.prcomps[, -1], graph = F)
cache("aggregate.pca") 
