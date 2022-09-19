#####Parameters for this script
#Decide whether to show cluster ellipses
show.ellipses = TRUE
#Decide whether to label them
label.clusters = TRUE
clusters.colours = c("Darkturquoise",
                     "Darkolivegreen4",
                     "darkviolet",
                     "Deeppink1",
                     "Sienna2",
                     "Grey28")
output_folders <- c("./data_out2022/")

#Change the names of the colors of clusters
cluster.colour.names = c("Aqua", "Olive", "Violet", "Pink", "Brown", "Grey")
# decide the number of clusters - it is 6 for SFR 2022.
num.clusters <-
  list(
    Environmental = 6,
    Political = 6,
    Economic = 6,
    Security = 6,
    Societal = 6,
    Human = 6
  )
num.clusters[[last(all.pca)[[1]]$dimension]] = 6
if (!dir.exists("./graphs/biplots"))
{
  dir.create("./graphs/biplots")
}
source("./lib/funcs.R")
biplot.pca = function(pca,
                      colour.clusters,
                      num.clusters,
                      axes.lims = NULL,
                      save.plot = T) {
  tmp = cluster(pca, num.clusters)
  p <-
    ggbiplot.sfr(
      pca,
      obs.scale = 1,
      var.scale = 1,
      ellipse = show.ellipses,
      circle = F,
      labels = tmp$labels$iso3c,
      groups = tmp$labels$cluster,
      var.axes = T,
      coloured.clusters = colour.clusters
    )
  p <- p + theme(legend.position = "none")
  p <- p + ggtitle(paste(pca$dimension))
  tmp$centroids$colour = cluster.colour.names[tmp$centroids$rank]
  if (label.clusters) {
    p <-
      p + geom_text(data = tmp$centroids, aes(x = PC1, y = PC2, label = colour))
  }
  if (pca$dimension == "Aggregate") {
    p <-
      p + geom_vline(xintercept = -1.2,
                     linetype = "dashed",
                     color = "red")
    p <-
      p + geom_vline(xintercept = -2.5,
                     linetype = "dashed",
                     color = "red")
    p <-
      p + annotate(
        "text",
        x = -1.4,
        y = -5,
        label = "Fragile",
        angle = 90
      )
    p <-
      p + annotate(
        "text",
        x = -2.7,
        y = -5,
        label = "Extremely Fragile",
        angle = 90
      )
    
  }
  p <- oecd.biplot(p, coloured.clusters = clusters.colours,
                   n = num.clusters)
  if (!is.null(axes.lims)) {
    p <- p + xlim(c(axes.lims$min.value.x, axes.lims$max.value.x))
    p <-
      p + ylim(c(axes.lims$min.value.y, axes.lims$max.value.y))
  }
  if (save.plot) {
    ggsave(
      p,
      filename = paste0(
        "./graphs/biplots/",
        pca$dimension,
        " ",
        " ",
        num.clusters,
        " clusters.pdf"
      ),
      height = 8,
      width = 10
    )
    ggsave(
      p,
      filename = paste0(
        "./graphs/biplots/",
        pca$dimension,
        " ",
        " ",
        num.clusters,
        " clusters.png"
      ),
      height = 8,
      width = 10
    )
  }
  
  return(p)
}

axes = NULL
for (i in names(all.pca)) {
  temp = all.pca[[i]]
  for (j in names(temp)) {
    pca = temp[["2021"]]
    axes = rbind(axes, data.frame(
      dimension = i,
      pc1 = pca$x[, 1],
      pc2 = pca$x[, 2]
    ))
  }
}

axes = axes %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(
    min.value.x = min(pc1),
    max.value.x = max(pc1),
    min.value.y = min(pc2),
    max.value.y = max(pc2)
  )
axes = split(axes, factor(axes$dimension))
for (i in names(all.pca)) {
  temp = all.pca[[i]]
  for (j in last(names(temp))) {
    pca = temp[[j]]
    p = biplot.pca(pca, fragile.clusters[[pca$dimension]], num.clusters[[pca$dimension]],
                   axes[[i]])
  }
}
rmExcept(
  c(
    "all.pca",
    "clusters.colours",
    "cluster.colour.names",
    "raw.data",
    "final_results_output",
    "frag_threshold",
    "ext_frag_threshold",
    "sfr.time.series",
    "final_results_timeseries"
  )
)