library(sf)
library(readr)
library(dplyr)

out_dir <- "data/pitch_shapefiles"

poly  <- st_read(file.path(out_dir, "pitch_polygon.shp"), quiet = TRUE)
lines <- st_read(file.path(out_dir, "pitch_lines.shp"), quiet = TRUE)
pts   <- st_read(file.path(out_dir, "pitch_points.shp"), quiet = TRUE)

passes <- read_csv(
  "data/modric_world_cup_knockout_passes.csv",
  show_col_types = FALSE
)

passes_plot <- passes %>%
  mutate(
    x_start_plot = start_x * 1.2,
    y_start_plot = 80 - (start_y * 0.8),
    x_end_plot   = end_x * 1.2,
    y_end_plot   = 80 - (end_y * 0.8),
    pass_col = ifelse(
      grepl("accurate", tag_labels) & !grepl("not accurate", tag_labels),
      "gold",
      "red"
    )
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
  main = "Luka Modrić Pass Map world cup 2018"
)

plot(st_geometry(lines), col = "white", lwd = 2, add = TRUE)
plot(st_geometry(pts), col = "white", pch = 16, cex = 1.1, add = TRUE)

arrows(
  x0 = passes_plot$x_start_plot,
  y0 = passes_plot$y_start_plot,
  x1 = passes_plot$x_end_plot,
  y1 = passes_plot$y_end_plot,
  col = passes_plot$pass_col,
  length = 0.07,
  lwd = 1.4
)

points(
  passes_plot$x_start_plot,
  passes_plot$y_start_plot,
  col = passes_plot$pass_col,
  pch = 19,
  cex = 0.5
)

legend(
  "topleft",
  legend = c("Accurate", "Not accurate"),
  col = c("gold", "red"),
  lwd = 2,
  bty = "n",
  text.col = "black"
)