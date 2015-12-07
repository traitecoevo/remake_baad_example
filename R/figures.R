download_baad <- function(destination_filename) {
  url <-
    "https://github.com/dfalster/baad/releases/download/v1.0.0/baad.rds"
  download(url, destination_filename, mode="wb")
}

download_michael <- function(destination_filename) {
  url <-
    "https://raw.githubusercontent.com/dfalster/baad/master/extra/baad.png"
  download(url, destination_filename, mode="wb")
}

my_cols <- function() {
  c(b_blue = "#6baed6", b_grey = "#969696", b_purple = "#9e9ac8", b_green = "#74c476",
    b_pink = "#e377c2", b_orange = "#FD8D3C", b_darkgrey = "#2D2D2D", b_white = "navajowhite4")
}

baad_variable_count <- function(baad) {

  par(oma = c(0, 0, 0, 0), mar = c(4, 13, 0, 1.5))
  cols <- my_cols()

  data <- baad[["data"]][, sapply(baad[["data"]], is.numeric) & !names(baad[["data"]]) %in%
    c("latitude", "longitude", "map", "mat", "lai")]

  records <- sort(sapply(data, function(x) sum(!is.na(x))))
  records <- records[records > 500]
  lab <- baad$dictionary$label[match(names(records), baad$dictionary$variable)]

  barplot(records, horiz = TRUE, las = 1, names.arg = lab, cex = 1, cex.names = 1,
    col = cols["b_orange"], border = NA, xlim = c(0, 20000))
  mtext("Number of records", 1, line = 3, cex = 1)
}

allometry_LH <- function(baad) {

  cols <- my_cols()

  data <- baad[["data"]]
  data <- subset(data, !is.na(data[["a.lf"]] * data[["h.t"]]))

  # remove species with < 3 data points
  n <- table(data$species)
  data <- subset(data, !data[["species"]] %in% names(n)[n < 4])

  par(mar = c(5, 5, 1, 1))

  A <- data[["a.lf"]]
  H <- data[["h.t"]]
  G <- data[["species"]]

  ax <- seq(-5, 4, by = 2)
  lab <- do.call(expression, lapply(ax, function(i) bquote(10^.(i))))

  smfit <- sma(H ~ A * G, log = "xy")

  blank_plot(xlim = c(1e-05, 5000), ylim = c(0.001, 200), log = "xy")
  axis(1, at = 10^ax, labels = lab, las = 1, cex.axis = 0.8)
  axis(2, at = 10^ax, labels = lab, las = 1, cex.axis = 0.8)

  plot(smfit, add = TRUE, col = cols["b_grey"], pch = 19, lwd = 0, cex = 1, type = "p")
  plot(smfit, add = TRUE, col = cols["b_green"], lwd = 1, type = "l", p.lines.transparent = 0.15)
  species <- G[!is.na(H * A)]
  mtext("Individual's height (m)", 2, line = 3)
  mtext(expression("Individual's leaf area" ~ ~(m^2)), 1, line = 3)
  legend("topleft", legend = paste(length(species), " individuals\n", length(unique(species)),
    " species"), bty = "n", cex = 0.5)
}

figure_map <- function(baad) {
  data <- baad$data

  cols <- make_transparent(my_cols(), 0.67)
  vegs <- c("BorF", "Sav", "TempF", "TempRF", "TropRF", "TropSF", "Wo")
  vegs_labels <- c("Boreal forest", "Savannah", "Temperate forest", "Temperate rainforest",
    "Tropical rainforest", "Tropical seasonal forest", "Woodland", "Glasshouse, Common garden")

  map("world", col = "grey80", bg = "white", lwd = 0.5, fill = TRUE, resolution = 0,
    wrap = FALSE, border = "grey80", ylim = c(-90, 90), mar = c(0,0,0,0))
  map("world", col = "black", boundary = TRUE, lwd = 0.2, interior = FALSE,
    fill = FALSE, add = TRUE, resolution = 0, wrap = TRUE)

  for (i in seq_len(length(vegs))) {
    if(i < 8) {
      data_i <- subset(data, data$vegetation == vegs[i])
    } else {
      data_i <- subset(data, data$growingCondition == "GH")
    }
    latlon <- with(data_i, paste(latitude, longitude))
    lat <- data_i$latitude[!duplicated(latlon)]
    lon <- data_i$longitude[!duplicated(latlon)]

    n <- table(latlon)
    symbols(lon, lat, circles = log10(n), inches = 0.1, fg = "black", bg = cols[i],
      add = TRUE)
 }

  # Add legend
  par(xpd = NA)
  legend(-190, 40, vegs_labels[1:8], fill = cols[1:8], bty = "n", cex = 0.8)
  ns <- c(10, 100, 1000)
  X <- rep(-180, 3)
  Y <- seq(-70, -50, by = 10)
  symbols(x = X, y = Y, circles = log10(ns), inches = 0.1, fg = "black", bg = my_cols()[1],
    add = TRUE)
  text(x = X + 5, y = Y, labels = as.character(ns), pos = 4)
  text(x = X, y = Y[3] + 10, labels = "plants", pos = 4)
}
