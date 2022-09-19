
source("./lib/funcs.R")
library(NbClust)
factr = function (x, y = unique(x)) 
{
    factor(x, levels = y, ordered = T)
}

biplot.pca = function(pca, colour.clusters, 
                      num.clusters, save.plot = T){
    tmp = cluster(pca, num.clusters)
    p <- ggbiplot.sfr(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = F, 
                      labels = tmp$labels$iso3c, 
                      groups = tmp$labels$cluster, var.axes = T, 
                      coloured.clusters = colour.clusters)
    p <- p + theme(legend.position = "none")
    p <- p + ggtitle(paste(pca$dimension, pca$year))
    p <- p + geom_text(data = tmp$centroids, aes(x = PC1, y = PC2, label = cluster))
    p <- oecd.biplot(p, coloured.clusters = colour.clusters, 
                     n = num.clusters)
    if(save.plot){
        ggsave(p, filename = paste0("./graphs/biplots/", pca$dimension, " ",
                                    pca$year, 
                                    " ",
                                    num.clusters, " clusters.pdf"), height = 8, width = 10)
    }
    
    return(p)
}

agg = lapply(all.pca$Aggregate, function(x){
    x = data.frame(iso3c = x$iso3c, value = x$x[,1])
    return(x)
})
agg = bind_rows(agg, .id = "year")
extr.fragility = -2.5
fragility = -1.2
fragile = agg %>% dplyr::filter(year == max(year)) %>% 
    dplyr::mutate(fragile = value < fragility) %>% 
    dplyr::filter(fragile)
fragile = sfr.time.series %>% dplyr::filter(year == max(year), iso3c %in% fragile$iso3c)
fragile <- fragile %>% dplyr::select(iso3c, variablename, imputed) %>% 
    dplyr::arrange(iso3c, variablename)
# rename for the bi-plots
# create panel data
tmp <- fragile %>% distinct() %>% spread(variablename, imputed)

num.clusters <-NbClust(tmp[,-1], distance = "euclidean", min.nc=2, max.nc=10, 
             method = "ward.D", index = "all")
num.clusters$n = length(unique(num.clusters$Best.partition))

# calculate a pca
pca <- my.pca(tmp, "fragile", max(sfr.time.series$year))

tmp = cluster(pca, length(unique(num.clusters$Best.partition)))
tmp$labels$cluster = LETTERS[as.numeric(num.clusters$Best.partition)]
p <- ggbiplot.sfr(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = F, 
                  labels = tmp$labels$iso3c, 
                  groups = tmp$labels$cluster, var.axes = T, 
                  coloured.clusters = colour.clusters)
p <- p + theme(legend.position = "none")
p <- p + ggtitle(paste(pca$dimension, pca$year))
p <- p + geom_text(data = tmp$centroids, aes(x = PC1, y = PC2, label = cluster))
p <- oecd.biplot(p, coloured.clusters = LETTERS[1:3], 
                 n = num.clusters$n)
p
ggsave(p, filename = paste("./graphs/Fragile-Cluster-Biplot.pdf", sep = ""), height = 10, 
       width = 10)
plot.threshold = 0.6 #percentage of clusters to be greater than in plot
plot.threshold = 1#ceiling(plot.threshold * num.clusters)
temp = fragile %>% dplyr::rename(value = imputed)
temp = left_join(temp, tmp$labels %>% dplyr::select(iso3c, cluster))
#####Simple mean plot

mean.plot = temp %>% dplyr::group_by(variablename) %>%
    mutate(banded = (value - min(value))/diff(range(value))) 
mean.plot = mean.plot %>% ungroup() %>% 
    dplyr::group_by(cluster, variablename) %>%
    dplyr::summarise(mean.banded = mean(banded))
mean.plot = mean.plot %>% ungroup() %>% dplyr::group_by(variablename) %>%
    mutate(rank = rank(mean.banded))

mean.plot$type = "Risks" 
pos = grepl("\\(C\\)", mean.plot$variablename)
mean.plot$type[pos] = "Coping Capacity"
myPalette <- brewer.pal(11, "Spectral")[c(10,4,2)]
mean.plot$rank = factr(mean.plot$rank)




#######
temp = split(temp, factor(temp$variablename))

signif = 0.95
#do an analysis of variance test and then a tukey test to 
#identify statistically significant difference between clusters
temp = lapply(temp, function(x){
    x = x %>% dplyr::filter(complete.cases(.))
    fit = aov(value ~ cluster, data = x)
    results = my.tukey.Test(fit)
    results = results %>% dplyr::filter(p.adj < 1 - signif)
    return(results)
})


library(igraph)
#plot graph of cluster dominance per variablename
degrees.bad = sapply(names(temp), function(i){
    df = temp[[i]]
    df$cluster1.1 = df$cluster1
    df$cluster2.1 = df$cluster2
    #if the difference if more than 0, i.e. for ppi cluster 1 scores worse than cluster 2
    pos = df$diff > 0 
    #switch the positions 
    df$cluster1[pos] = df$cluster2.1[pos]
    df$cluster2[pos] = df$cluster1.1[pos]
    g = graph_from_edgelist(as.matrix(df %>% select(cluster1, cluster2)) )
    #plot(g, main = i)
    degrees = degree(g, mode = "in", loops = TRUE, normalized = FALSE)
    degrees = data.frame(cluster = names(degrees), degree = degrees)
    degrees = degrees %>% arrange(desc(degree))
    return(degrees)
}, simplify = F)

degrees.bad = bind_rows(degrees.bad, .id = "variablename")
degrees.bad$degree = - degrees.bad$degree

degrees.good = sapply(names(temp), function(i){
    df = temp[[i]]
    df$cluster1.1 = df$cluster1
    df$cluster2.1 = df$cluster2
    #if the difference if more than 0, i.e. for ppi cluster 1 scores worse than cluster 2
    pos = df$diff < 0 
    #switch the positions 
    df$cluster1[pos] = df$cluster2.1[pos]
    df$cluster2[pos] = df$cluster1.1[pos]
    g = graph_from_edgelist(as.matrix(df %>% select(cluster1, cluster2)) )
    #plot(g, main = i)
    degrees = degree(g, mode = "in", loops = TRUE, normalized = FALSE)
    degrees = data.frame(cluster = names(degrees), degree = degrees)
    degrees = degrees %>% arrange(desc(degree))
    return(degrees)
}, simplify = F)

degrees.good = bind_rows(degrees.good, .id = "variablename")

degrees = rbind(degrees.good, degrees.bad)
degrees = degrees %>% dplyr::filter(abs(degree) >= plot.threshold)

temp = expand.grid(variablename = unique(degrees$variablename), 
                   cluster = unique(degrees$cluster))

degrees = left_join(temp, degrees)
degrees$degree[is.na(degrees$degree)] = 0
degrees$type = "Risks" 
pos = grepl("\\(C\\)", degrees$variablename)
degrees$type[pos] = "Coping Capacity"
#myPalette <- colorRampPalette((brewer.pal(11, "RdBu")), space="Lab")
#TODO: Check this
excludes = degrees %>% dplyr::group_by(variablename) %>% dplyr::summarise(num.zeros = sum(degree==0)) %>%
    dplyr::filter(num.zeros == num.clusters$n - 1)
excludes2 = data.frame(variablename = unique(sfr.time.series$variablename), num.zeros = num.clusters$n)


degrees2 = degrees %>% dplyr::filter(!(variablename %in% excludes$variablename))
excludes2 = excludes2 %>% dplyr::filter(!(variablename %in% degrees2$variablename))
pos = !(mean.plot$variablename %in% excludes2$variablename)
mean.plot$variablename[pos] = paste0(mean.plot$variablename[pos], "*")
mean.plot = as.data.frame(mean.plot)
p = ggplot(mean.plot, aes(x = cluster, y = variablename)) +
    geom_tile(aes(fill = rank), alpha = 0.8) + facet_wrap(~type, ncol = 1, scales = "free") +
    scale_fill_manual(values = rev(myPalette), name = "Global Comparison",
                      labels = c("Weakest", #rep("", 6),
                                 "Second Weakest", #rep("", 6),
                                 "Stronger")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(keywidth = 4, keyheight = 1, 
                               title.position = "top",
                               title.hjust = 0.5,
                               label.position = "bottom",
                               label.hjust = 0.5)) +
    ylab("") +
    labs(title = "Cluster Comparisons",
         subtitle = "* indicators show difference significant at 95% Confidence") +
    theme(plot.caption = element_text(hjust = 0))
p
ggsave(p, filename = paste("./graphs/Fragile-Cluster-Indicators.pdf", sep = ""), height = 10, 
       width = 10)




#get regions
library(WDI)
regions = as.data.frame(WDI::WDI_data$country) %>% dplyr::select(iso3c, region, income)
regions$region = gsub(" \\(all income levels\\)", "", regions$region)
regions$income = gsub(": nonOECD", "", regions$income)
regions$income = gsub(": OECD", "", regions$income)
regions$income = factr(regions$income, unique(regions$income)[c(4,3,2,1, 5, 6)])
regions = left_join(tmp, regions)

region = regions %>% dplyr::group_by(cluster, region) %>% dplyr::summarise(n = n()) %>%
    ungroup() %>% dplyr::group_by(cluster) %>% dplyr::mutate(pc = n/sum(n))

p = ggplot(region, aes(x = region, y = pc)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
    scale_y_continuous(labels = scales::percent) + facet_wrap(~cluster, ncol = 2) + 
    labs(y = "Percentage of Countries",
         title = "Regions of clusters") +
    theme(axis.text.x = element_text(angle = 65, hjust = 1))
p = oecd.plot(p)
ggsave(p, filename = paste("./graphs/Cluster-Regions.pdf", sep = ""), height = 10, 
       width = 10)

region = regions %>% dplyr::group_by(cluster, income) %>% dplyr::summarise(n = n()) %>%
    ungroup() %>% dplyr::group_by(cluster) %>% dplyr::mutate(pc = n/sum(n))
region$income = factr(region$income, levels(region$income)[c(4,1,2,3,6,5)])

p = ggplot(region, aes(x = income, y = pc)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
    scale_y_continuous(labels = scales::percent) + facet_wrap(~cluster, ncol = 2) + 
    labs(y = "Percentage of Countries",
         title = "Income of clusters") +
    theme(axis.text.x = element_text(angle = 65, hjust = 1))
p = oecd.plot(p)
ggsave(p, filename = paste("./graphs/Cluster-Income.pdf", sep = ""), height = 10, 
       width = 10)

