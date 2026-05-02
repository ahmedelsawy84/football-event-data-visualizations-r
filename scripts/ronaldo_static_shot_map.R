library(sf)

out_dir <- "C:/Users/ahmed/Desktop/statsbomb_pitch_shapefiles"

poly  <- st_read(file.path(out_dir, "pitch_polygon.shp"), quiet = TRUE)
lines <- st_read(file.path(out_dir, "pitch_lines.shp"), quiet = TRUE)
pts   <- st_read(file.path(out_dir, "pitch_points.shp"), quiet = TRUE)

plot(st_geometry(poly), col = c("darkgreen", "grey20", "grey20"), border = NA, reset = FALSE)
plot(st_geometry(lines), col = "white", lwd = 2, add = TRUE)
plot(st_geometry(pts), col = "white", pch = 16, cex = 1.2, add = TRUE)

#--------------------------------------------------------------------------

library(sf)
library(readr)
library(dplyr)





# Read Cristiano Ronaldo shots CSV

shots <- read_csv(
  "data/cristiano_ronaldo_shots_extracted.csv",
  show_col_types = FALSE
)


# 3) Convert Wyscout coordinates -> StatsBomb-style pitch
#    Wyscout: x,y in 0-100
#    StatsBomb pitch: x in 0-120, y in 0-80
#
#    Base plotting uses a normal Cartesian y-axis,
#    so we flip y vertically for display.

shots_plot <- shots %>%  # create nw data frame named shots_plot %>% take the result on the left and pass it to the next funxtion 
  mutate( # adds or change columns in data frame 
    x_plot = x * 1.2,
    y_plot = 80 - (y * 0.8),
    point_col = ifelse(is_goal == 1, "gold", "red"), # if is_goal = 1 the shot colored gold else it is red
    point_pch = ifelse(is_goal == 1, 19, 19), # 19 means filled circle
    point_cex = ifelse(is_goal == 1, 1.3, 1.1) # goal size is bigger
  )


# Plot the pitch

plot(
  st_geometry(poly),
  col = c("darkgreen", "grey20", "grey20"),
  border = c(NA, "white", "white"),
  xlim = c(-2, 122),
  ylim = c(0, 80),
  asp = 1,
  reset = FALSE,
  axes = FALSE,
  main = "Cristiano Ronaldo Shot Map"
)

plot(
  st_geometry(lines),
  col = "white",
  lwd = 2, # ,ake the line thicker
  add = TRUE
)

plot(
  st_geometry(pts),
  col = "white",
  pch = 16,
  cex = 1.1,
  add = TRUE
)


#  Add Ronaldo shot points
points(
  shots_plot$x_plot,
  shots_plot$y_plot,
  col = shots_plot$point_col,
  pch = shots_plot$point_pch,
  cex = shots_plot$point_cex
)


#  Add legend

legend(
  "topleft",
  legend = c("Goal", "No goal"),
  col = c("gold", "red"),
  pch = c(19, 19),
  pt.cex = c(1.3, 1.1),
  bty = "n",
  text.col = "black"
)



