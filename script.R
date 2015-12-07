library(smatr)
library(maps)
library(mapdata)
library(downloader)
source("R/figures.R")
source("R/utils.R")

dir.create("downloads", FALSE, TRUE)
dir.create("figures", FALSE, TRUE)
dir.create("tables", FALSE, TRUE)

download_michael("downloads/michael.png")
download_baad("downloads/baad.rds")
baad <- readRDS("downloads/baad.rds")

pdf("figures/map.pdf", width = 9L, height = 4L)
figure_map(baad)
dev.off()

pdf("figures/baad_variable_count.pdf", width = 6L, height = 6L)
figure_variable_count(baad)
dev.off()

pdf("figures/baad_leaf_height.pdf", width = 3L, height = 3L)
figure_allometry_LH(baad)
dev.off()

summary <- table_summary(baad)
write.csv(summary, "tables/summary.csv", row.names = FALSE)
