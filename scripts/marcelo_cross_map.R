library(sf)
library(readr)
library(dplyr)

out_dir <- "data/pitch_shapefiles"

poly  <- st_read(file.path(out_dir, "pitch_polygon.shp"), quiet = TRUE)
lines <- st_read(file.path(out_dir, "pitch_lines.shp"), quiet = TRUE)
pts   <- st_read(file.path(out_dir, "pitch_points.shp"), quiet = TRUE)

crosses <- read_csv(
  "data/marcelo_crosses.csv",
  show_col_types = FALSE
)

crosses_plot <- crosses %>%
  mutate(
    x_plot = start_x * 1.2,
    y_plot = 80 - (start_y * 0.8),
    point_col = ifelse(grepl("accurate", tag_labels) &
                         !grepl("not accurate", tag_labels),
                       "gold", "red")
  )

plot(
  st_geometry(poly),
  col = c("darkgreen", "grey20", "grey20"),
  border = c(NA, "white", "white"),
  xlim = c(-2, 122),
  ylim = c(0, 80),
  asp = 1,
  reset = FALSE,
  axes = FALSE,
  main = "Marcelo Cross Starting Locations"
)

plot(st_geometry(lines), col = "white", lwd = 2, add = TRUE)
plot(st_geometry(pts), col = "white", pch = 16, cex = 1.1, add = TRUE)

points(
  crosses_plot$x_plot,
  crosses_plot$y_plot,
  col = crosses_plot$point_col,
  pch = 19,
  cex = 1.2
)

legend(
  "topleft",
  legend = c("Accurate cross", "Not accurate cross"),
  col = c("gold", "red"),
  pch = 19,
  bty = "n",
  text.col = "black"
)