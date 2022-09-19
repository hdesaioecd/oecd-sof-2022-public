cluster = function(pca,
                   num.clusters = 6,
                   cluster.method = "ward.D2",
                   round.numbers = T) {
  tmp <- data.frame(iso3c = pca$iso3c, pca$x[, 1:2])
  distance <- dist(tmp[,-1], method = "euclidean")
  if (round.numbers) {
    distance <- round(dist(tmp[,-1], method = "euclidean"), digits = 2)
  }
  d2 <- as.data.frame(as.matrix(distance))
  d2$col <- rownames(d2)
  d2 <- d2 %>% gather(row, dist,-col)
  clusters <- hclust(distance, method = cluster.method)
  clusters <- cutree(clusters, num.clusters)
  # calculate centroids for the visualisation
  centroids <-
    as.data.frame(apply(tmp[,-1], 2, function(x)
      tapply(x, clusters, mean)))
  centroids$dist <- with(centroids, sqrt(PC1 ^ 2 + PC2 ^ 2))
  centroids$omega <- atan(centroids$PC2 / centroids$PC1)
  centroids$cut <- LETTERS[1:nrow(centroids)]
  centroids$rank <- rank(centroids$PC1)
  centroids$cluster <- LETTERS[centroids$rank]
  tmp$cut <- LETTERS[clusters]
  tmp <-
    dplyr::left_join(tmp, select(centroids, cut, cluster), by = "cut")
  tmp = list(labels = tmp, centroids = centroids)
  return(tmp)
}

my.tukey.Test = function(fit) {
  results = TukeyHSD(fit)
  #plot(results)
  results = results[[1]]
  results = data.frame(pairs = row.names(results), results)
  results = results %>% separate(pairs, c("cluster1", "cluster2"), sep = "-")
  results$p.adj = round(results$p.adj, 4)
  return(results)
}

extend.time.series = function(df, replace.with = NULL) {
  require(padr)
  df$date = as.Date(paste0(df$year, "-01-01"))
  df = df %>%
    group_by(iso3c, variablename) %>%
    pad(
      .,
      start_val = min(df$date),
      end_val = max(df$date),
      interval = "year"
    )
  df$year = year(df$date)
  df = df %>% select(-date)
  if (!is.null(replace.with)) {
    df$value[is.na(df$value)] = replace.with
  }
  return(df)
}

interpolate.data <- function(df) {
  # linear interpolation
  
  # need at least 2 non-na values to interpolate
  if (nrow(df[!is.na(df$value), ]) > 0) {
    xout <- 1:nrow(df)
    y <- df$value
    interpolation <-
      approx(
        x = xout[!is.na(y)],
        y = y[!is.na(y)],
        xout = xout,
        rule = 2
      )
    df$yhat <- interpolation$y
    #fix for interploation for only 1 dfset of consecutive years
    df$yhat = ifelse(is.na(df$yhat), df$value, df$yhat)
  }
  
  return(df)
}

#' Interpolates data
#'
#' This is a wrapper function takes a data frame and fills in
#' interpolated and extrapolated data for the whole time series
#'
#' @param df dataframe in iep format
#'
#' @return Returns list with filled in time series, column yhat is the interpolated value.
#' Please check original value with yhat column to make sure you are happy with the results
#'
#' @examples data(rawdata)
#' df <- iep.interpolate(rawdata)
#'
#' @keywords imputation
#' @author Dave
#' @export
interpolate = function(df) {
  require(tidyverse)
  require(padr)
  require(lubridate)
  require(testthat)
  #need to add another variable for entries with only one country-year-car value,
  #interpolate breaks otherwise
  if (diff(range(df$year)) > 0) {
    num.years.test = df %>%
      dplyr::group_by(iso3c, variablename) %>%
      dplyr::summarise(
        only.one.yr = min(year) == max(year),
        year = min(year),
        value = max(value)
      ) %>%
      filter(only.one.yr)
    num.years.test$year = ifelse(num.years.test$year == min(df$year),
                                 max(df$year),
                                 min(df$year))
    df = bind_rows(df, num.years.test[, names(df)])
    
    #extend data frame
    df = extend.time.series(df)
    
    #test
    num.years.test = df %>%
      dplyr::group_by(iso3c, variablename) %>%
      dplyr::summarise(only.one.yr = min(year) == max(year))
    expect_that(sum(num.years.test$only.one.yr), equals(0),
                info = "You Have Only One Entry for Interpolation")
    #interpolate
    df <- df %>%
      dplyr::group_by(iso3c, variablename) %>% do(interpolate.data(.))
    df = as.data.frame(df)
  } else{
    message("***You only have one year of data across all indicators")
    message("***No interpolation or extrapolation possible or needed")
    message("***Original data frame returned untouched")
  }
  
  return(df)
}

add.zeros.for.missing.countries = function(x, raw.data, replace.with = 0)
{
  ####add zeros for missing values
  temp = lapply(raw.data[-1], function(x)
    return(data.frame(iso3c = unique(
      as.character(x$iso3c)
    ))))
  temp = bind_rows(temp)
  temp = unique(country.code.name(unique(as.character(temp$iso3c))))
  temp = setdiff(temp, x$iso3c)
  temp = data.frame(
    iso3c = temp,
    variablename = x$variablename[1],
    year = max(as.numeric(as.character(x$year))),
    value = replace.with
  )
  temp = temp %>% dplyr::filter(complete.cases(.))
  x = bind_rows(x, temp)
  x
}

most.recent = function(df)
{
  df = df %>% dplyr::filter(complete.cases(value))
  df <- df %>%
    dplyr::group_by(iso3c, variablename) %>%
    dplyr::filter(year == max(year)) %>% ungroup()
}

pca.transform = function(raw.data, method = "pca")
{
  ###################
  # This function performs a PCA on the indicators within a dimension-type couplet
  ##################
  require(caret)
  #Select the relevant columns
  x = raw.data %>% dplyr::select(dimension, type, iso3c, variablename, imputed) %>% distinct()
  #Create a list of the data by dimension-type couplet
  temp = split(x, factor(x$dimension))
  temp = lapply(temp, function(x)
    split(x, factor(x$type)))
  variance = data.frame(expand.grid(c("Coping", "Risk"), names(temp)))
  print(variance)
  variance$variance = NA
  #For each dimension-type couplet, calculate the first principal component
  counter = 1
  test = lapply(temp, function(y)
  {
    return(lapply(y, function(x)
    {
      #Because pricipal components sometimes flips the direction of measures,
      #we need to keep a record of the original direction of the measures
      direction = x %>% dplyr::group_by(dimension, type, variablename) %>%
        dplyr::mutate(score = (imputed - min(imputed)) / diff(range(imputed))) %>% ungroup()
      direction = direction %>% dplyr::group_by(iso3c) %>% dplyr::summarise(av = mean(score), worst.case = max(score))
      #select relevant columns for PCA
      x = x %>% dplyr::select(-c(dimension, type)) %>% spread(variablename, imputed)
      rownames(x) = x$iso3c
      x = x %>% dplyr::select(-iso3c)
      #perform PCA
      if (ncol(x) > 1)
        #some dimension-type couplet only have 1 indicator which throws up an error in the PCA
      {
        ir.pca <- prcomp(x, center = TRUE, scale. = TRUE)
        variance$variance[counter] <<-
          percent(summary(ir.pca)$importance[2, 1])
        counter <<- counter + 1
        test = as.data.frame(ir.pca$x)
        test$CI = ci_factor(as.data.frame(x), method = "ALL")[[1]]
      } else
      {
        names(x) = "PC1"
        test = x
        print(paste("Variance Explained:", percent(1)))
      }
      #by correlating orginal direction of metrics and the PCA we can reallign both
      test = data.frame(iso3c = rownames(test),
                        PC1 = test$PC1,
                        CI = test$CI)
      direction = dplyr::left_join(test, direction)
      correl = as.data.frame(cor(select(direction, -c(iso3c, CI))))
      correl = as.numeric(correl$PC1)
      correl = correl[correl < 1]
      pos = which(abs(correl) == max(abs(correl)))[1]
      if (method == "pca")
      {
        test$PC1 = test$PC1 * sign(correl[pos])
      }
      if (method == "mean")
      {
        test = dplyr::left_join(test, direction)
        test$PC1 = test$av
      }
      if (method == "worst.case")
      {
        test = dplyr::left_join(test, direction)
        test$PC1 = test$worst.case
      }
      
      if (method == "factor")
      {
        test = dplyr::left_join(test, direction)
        test$PC1 = test$CI
      }
      
      return(test)
    }))
  })
  #Create a data frame with the PCA scores
  result = NULL
  for (i in names(test))
  {
    for (j in names(test[[i]]))
    {
      x = test[[i]][[j]]
      x = data.frame(
        iso3c = x$iso3c,
        dimension = i,
        type = j,
        score = x$PC1
      )
      result = rbind(result, x)
    }
  }
  #return result
  print(variance)
  write.csv(variance, file = "./data_out2017/pca_variance.csv", row.names = F)
  result
}

impute = function(raw.data, use.precomputed = F)
{
  ############
  #This imputes missing data points by its 15 nearest neighbors
  ############
  require(caret)
  if (!use.precomputed) {
    #Select relevant columns and distinct rows due to indicator double ups
    x = raw.data %>%
      dplyr::select(iso3c, variablename,  value) %>%
      distinct()
    #create a uid dataframe
    ids = data.frame(
      variablename = unique(x$variablename),
      uid = paste("a", 1:length(unique(x$variablename)), sep = "")
    )
    ismorebetter = raw.data %>%
      dplyr::select(variablename, doesmoreincreasefragility) %>%
      distinct(.)
    #combine ids with x
    x <- left_join(x, ids, "variablename")
    x <- x %>% select(-variablename) %>%
      pivot_wider(names_from = uid,
                  values_from = value) %>%
      as.data.frame()
    #perform knn imputation
    preObj = preProcess(x[, -1], method = "knnImpute", k = 15)
    #rescale back to original measurement scale
    temp = predict(preObj, x[, -1])
    temp = sweep(temp, MARGIN = 2, preObj$std, `*`)
    temp = sweep(temp, MARGIN = 2, preObj$mean, `+`)
    #check correlations
    test.imputations(x[, -1], temp)
    #create imputed data frame
    temp = data.frame(iso3c = x$iso3c, temp)
    temp = temp %>% 
      gather(uid, imputed,-iso3c)
    ids = left_join(ids, ismorebetter)
    temp = left_join(temp, ids)
    tmp = distinct(select(raw.data, dimension, type, variablename))
    temp = left_join(temp, tmp)
    temp =
      left_join(temp, raw.data)
    temp = temp %>%
      dplyr::select(-uid)
    temp = temp %>%
      group_by(variablename) %>%
      mutate(max.year = max(year, na.rm =
                              T)) %>%
      ungroup()
    pos = is.na(temp$year)
    temp$year[pos] = temp$max.year[pos]
    temp$country = country.code.name(temp$iso3c)
    temp = temp %>% dplyr::select(-max.year)
  } else{
    load("./cache/pre-computed-imputations/pre-computed-imputations.RData")
    temp = raw.data
    temp$imputed = temp$value
    tempid = as.character(paste0(temp$iso3c, temp$variablename, temp$dimension))
    precomputedid = paste0(
      pre.computed.imputations$iso3c,
      pre.computed.imputations$variablename,
      pre.computed.imputations$dimension
    )
    pre.computed.imputations = pre.computed.imputations[precomputedid %in% setdiff(precomputedid, tempid), ]
    temp = rbind(temp[, names(pre.computed.imputations)], pre.computed.imputations)
    
    
  }
  
  ###test each country has a data point
  availability = as.data.frame(table(temp$iso3c, temp$variablename))
  availability = availability %>% group_by(Var1) %>% dplyr::summarise(n = n(), missing = sum(Freq ==
                                                                                               0) / n())
  expect_that(max(availability$n), equals(length(unique(
    temp$variablename
  ))))
  expect_that(min(availability$n), equals(length(unique(
    temp$variablename
  ))))
  expect_that(max(availability$missing), equals(0))
  tmp = temp %>% dplyr::select(iso3c, variablename, imputed) %>% distinct() %>%
    tidyr::spread(variablename, imputed)
  expect_that(sum(is.na(tmp)), equals(0))
  
  return(temp)
}

test.imputations = function(raw, imputed)
{
  require(scales)
  orig = cor(raw, use = "pairwise.complete.obs")
  new = cor(imputed, use = "pairwise.complete.obs")
  diff.cor = (new - orig)
  pos = which(abs(diff.cor) == max(abs(diff.cor), na.rm = T))
  diff.cor = diff.cor[pos[1]]
  orig = colMeans(raw, na.rm = T)
  new = colMeans(imputed)
  diff.mean = (new - orig) / orig
  pos = which(abs(diff.mean) == max(abs(diff.mean)))
  diff.mean = diff.mean[pos]
  orig = apply(raw, 2, function(x)
    sd(x, na.rm = T))
  new = apply(imputed, 2, function(x)
    sd(x, na.rm = T))
  diff.sd = (new - orig) / orig
  pos = which(abs(diff.sd) == max(abs(diff.sd)))
  diff.sd = diff.sd[pos]
  test = data.frame(
    max.diff.mean = diff.mean,
    max.diff.sd = diff.sd,
    max.diff.r = diff.cor
  )
  test = round(test, 2)
  test$max.diff.mean = percent(test$max.diff.mean)
  test$max.diff.sd = percent(test$max.diff.sd)
  print("Imputation Statistics")
  print(test)
}


country.code.name = function(x, oecd = F)
{
  require(countrycode)
  if (max(nchar(x), na.rm = T) == 3)
  {
    y = countrycode(
      x,
      "iso3c",
      "iso.name.en",
      custom_match = c(
        "KSV" = "Kosovo",
        "XKX" = "Kosovo",
        "UVK" = "Kosovo",
        "CHI" = "Channel Islands",
        "SWZ" = "Eswatini",
        "KOS" = "Kosovo",
        "XKO" = "Kosovo",
        "ADO" = "Andorra",
        "ANT" = "Netherlands Antilles",
        "ROM" = "Romania",
        "TMP" = "Timor-Leste",
        "WBG" = "West Bank and Gaza Strip",
        "ZAR" = "Congo, Dem. Rep.",
        "CHA" = "Chad",
        "SCG" = "Serbia and Montenegro",
        "TUR" = "Türkiye"
      )
    )
    pos = is.na(y)
    y[pos] = countrycode(x[pos], "wb", "iso.name.en")
  } else
  {
    if (oecd)
    {
      y = oecd.country.name(x)
    } else
    {
      y = countrycode(
        x,
        "country.name",
        "iso3c",
        custom_match = c(
          "Kosovo" = "XKX",
          "Kosovo under UNSCR 1244" = "XKX",
          "Eswatini (Kingdom of)" = "SWZ",
          "Eswatini" = "SWZ",
          "eSwatini" = "SWZ",
          "Somaliland" = "SOM",
          "Micronesia" = "FSM",
          "Channel Islands" = "CHI",
          "Eswatini" = "SWZ",
          "Andorra" = "ADO",
          "Romania" = "ROM",
          "Netherlands Antilles" = "ANT",
          "West Bank and Gaza Strip" = "PSE",
          "Timor-Leste" = "TLS",
          "Congo, Dem. Rep." = "COD",
          "Congo, Rep." = "COG",
          "Aruba, Kingdom of the Netherlands" = "ABW",
          "Curaçao, Kingdom of the Netherlands" = "CUW",
          "Sint Maarten, Kingdom of the Netherlands" = "SXM",
          "Yemen (North Yemen)" = "YEM",
          "Yemen, North" = "YEM",
          "Cent Afr Rep" = "CAF",
          "Amer Samoa" = "WSM",
          "Fr Guiana" = "GUF",
          "Sudan South" = "SSD",
          "Micronesia (FS of)" = "FSM",
          "Türkiye" = "TUR"
        )
      )
      pos = is.na(y)
      y[pos] = countrycode(x[pos], 'country.name', 'wb')
    }
  }
  return(y)
}


oecd.country.name = function(x, short = F)
{
  oecd = read_excel("./data/additional data/OECD Country Names.xlsx")
  pos = match(x, oecd$ISO3)
  y = oecd$`Name in lists,`[pos]
  if (short)
    y = oecd$`Short name`[pos]
  return(y)
}

reorder.df = function(y)
{
  cols = c("iso3c", names(y)[names(y) != "iso3c"])
  y = y[, cols]
  y
}

oecd.biplot = function(p, coloured.clusters, n)
{
  cols = coloured.clusters
  p = p + scale_color_manual(values = cols)
  p = oecd.plot(p)
  return(p)
}


oecd.plot = function(p, coloured.clusters, n)
{
  p = p + theme(panel.background = element_rect(fill = rgb(233 / 255, 237 /
                                                             255, 247 / 255)))
  return(p)
}


ggbiplot.sfr = function (pcobj,
                         choices = 1:2,
                         scale = 1,
                         pc.biplot = TRUE,
                         obs.scale = 1 - scale,
                         var.scale = scale,
                         groups = NULL,
                         ellipse = FALSE,
                         ellipse.prob = 0.68,
                         labels = NULL,
                         labels.size = 3,
                         alpha = 1,
                         var.axes = TRUE,
                         circle = FALSE,
                         circle.prob = 0.69,
                         varname.size = 3,
                         varname.adjust = 1.5,
                         varname.abbrev = FALSE,
                         transparency = 0.5,
                         ...)
{
  #Modified by David Hammond from Original Package written by vqv
  #To install original package run:
  #library(devtools)
  #install_github("vqv/ggbiplot")
  library(ggplot2)
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = "*")
    v <-
      sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),
                                               1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x / nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d ^ 2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <-
    as.data.frame(sweep(u[, choices], 2, d[choices] ^ obs.scale,
                        FUN = "*"))
  v <- sweep(v, 2, d ^ var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <-
    sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u ^ 2)) ^ (1 / 4)
  v.scale <- rowSums(v ^ 2)
  df.v <- r * df.v / sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <-
    paste(u.axis.labs,
          sprintf(
            "(%0.1f%% explained var.)",
            100 * pcobj$sdev[choices] ^
              2 / sum(pcobj$sdev ^ 2)
          ))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180 / pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  g <-
    ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
    ylab(u.axis.labs[2]) + coord_equal()
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi,-pi,
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta),
                           yvar = r *
                             sin(theta))
      g <- g + geom_path(
        data = circle,
        color = muted("white"),
        size = 1 / 2,
        alpha = 1 / 3
      )
    }
    g <- g + geom_segment(
      data = df.v,
      aes(
        x = 0,
        y = 0,
        xend = xvar,
        yend = yvar
      ),
      arrow = arrow(length = unit(1 / 2,
                                  "picas")),
      color = brewer.pal(n = 9, "Greys")[9],
      alpha = transparency
    )
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups),
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    }
    else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi,-pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2,
                       mu, FUN = "+"),
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <-
      g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    g <- g + geom_text(
      data = df.v,
      aes(
        label = varname,
        x = xvar,
        y = yvar,
        angle = angle,
        hjust = hjust
      ),
      color = brewer.pal(9, "Greys")[9],
      size = varname.size,
      alpha = transparency
    )
  }
  return(g)
}

hpc.change <- function(all) {
  library(scales)
  all$year = as.numeric(all$year)
  all$value = as.numeric(all$value)
  temp <-
    expand.grid(
      iso3c = unique(all$iso3c),
      variablename = unique(all$variablename),
      from = unique(all$year),
      to = unique(all$year)
    )
  temp$num.years <- as.numeric(temp$to) - as.numeric(temp$from)
  temp <- subset(temp, num.years > 0)
  all <- subset(all, select = c(iso3c, year, variablename, value))
  names(all) <- c("iso3c", "from" , "variablename", "from.value")
  temp <- merge(temp, all)
  names(all) <- c("iso3c", "to" , "variablename", "to.value")
  temp <- merge(temp, all)
  temp$absolute.diff = round(with(temp, (to.value - from.value)), 3)
  temp$raw.proportional.change <-
    round(with(temp, (to.value - from.value) / from.value), 3)
  temp$annual.proportional.change <-
    round(with(temp, ((
      to.value / from.value
    ) ^ (1 / num.years)) - 1), 3)
  temp <- temp[, c(
    "iso3c",
    "variablename",
    "num.years",
    "from",
    "from.value",
    "to",
    "to.value",
    "absolute.diff",
    "raw.proportional.change",
    "annual.proportional.change"
  )]
  names(temp) <- c(
    "iso3c",
    "variablename",
    "num.years",
    "from",
    "from.value",
    "to",
    "to.value",
    "absolute.diff",
    "prop.growth",
    "annual.prop.growth"
  )
  temp$iso3c = as.character(temp$iso3c)
  temp$variablename = as.character(temp$variablename)
  return(temp)
}

'%!in%' <- Negate('%in%')

wb.func <- function(x) {
  x %>%
    remove_empty("cols") %>%
    clean_names() %>%
    select(-c(country_name, indicator_code, indicator_name)) %>%
    pivot_longer(names_to = "year",
                 values_to = "value",-c(country_code)) %>%
    rename(iso3c = country_code) %>%
    mutate(
      iso3c = country.code.name(iso3c),
      year = gsub("x", "", year),
      year = as.integer(year)
    ) %>%
    drop_na(iso3c) %>%
    drop_na(value)
}

population_func <- function(x){
  x %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "Country/Area") %>%
    dplyr::select(-c(index,variant,notes,country_code,type,parent_code)) %>%
    dplyr::rename(country = region_subregion_country_or_area) %>%
    tidyr::pivot_longer(names_to = "year",
                        values_to = "value",
                        -country) %>%
    dplyr::mutate(
      value = as.numeric(value) * 1000,
      year = gsub("x", "", year),
      year = as.integer(year),
      iso3c = country.code.name(country)
    ) %>%
    dplyr::rename(population = value) %>%
    dplyr::select(-country) %>%
    filter(year >= 2000)
}

DACdonors <- c(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12,
  18,
  20,
  21,
  22,
  40,
  50,
  61,
  68,
  69,
  75,
  76,
  301,
  302,
  701,
  742,
  801,
  820
)
