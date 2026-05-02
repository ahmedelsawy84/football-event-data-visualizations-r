library(readr)
library(dplyr)
library(stringr)
library(plotly)
library(htmlwidgets)

# --------------------------------------------------
# 1) Read Cristiano Ronaldo shots CSV
# --------------------------------------------------
shots <- read_csv(
  "data/cristiano_ronaldo_shots.csv",
           show_col_types = FALSE
)

# --------------------------------------------------
# 2) Prepare fields for plotting + hover
# --------------------------------------------------
shots_plot <- shots %>%
  mutate(
    # Wyscout -> StatsBomb-style pitch
    x_plot = x * 1.2,
    y_plot = 80 - (y * 0.8),
    
    # Extract opponent name from match label
    opponent = case_when(
      str_detect(match_label, "^Real Madrid - ") ~
        str_trim(str_extract(match_label, "(?<=^Real Madrid - ).*?(?=,)")),
      str_detect(match_label, " - Real Madrid,") ~
        str_trim(str_extract(match_label, "^.*?(?= - Real Madrid,)")),
      TRUE ~ match_label
    ),
    
    minute_label = paste0(floor(minute_decimal), "'"),
    
    on_target = case_when(
      is_goal == 1 ~ "Yes",
      is_accurate == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    shot_result = case_when(
      is_goal == 1 ~ "Goal",
      is_accurate == 1 ~ "On target",
      TRUE ~ "Off target"
    ),
    
    shot_distance = sqrt((120 - x_plot)^2 + (40 - y_plot)^2),
    
    hover_text = paste0(
      "<b>Opponent:</b> ", opponent,
      "<br><b>Minute:</b> ", minute_label,
      "<br><b>Result:</b> ", shot_result,
      "<br><b>On target:</b> ", on_target,
      "<br><b>Shot type:</b> ", shot_type,
      "<br><b>Distance:</b> ", round(shot_distance, 1),
      "<br><b>Match:</b> ", match_label
    )
  )

# --------------------------------------------------
# 3) Build interactive pitch directly in plotly
# --------------------------------------------------
fig <- plot_ly()

# Off target shots
fig <- fig %>%
  add_trace(
    data = filter(shots_plot, shot_result == "Off target"),
    x = ~x_plot,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    name = "Off target",
    text = ~hover_text,
    hovertemplate = "%{text}<extra></extra>",
    marker = list(
      color = "#E94F37",
      size = 6,
      opacity = 0.9,
      line = list(color = "white", width = 0.7)
    )
  )

# On target shots
fig <- fig %>%
  add_trace(
    data = filter(shots_plot, shot_result == "On target"),
    x = ~x_plot,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    name = "On target",
    text = ~hover_text,
    hovertemplate = "%{text}<extra></extra>",
    marker = list(
      color = "#17BEBB",
      size = 6,
      opacity = 0.9,
      line = list(color = "white", width = 0.7)
    )
  )

# Goals
fig <- fig %>%
  add_trace(
    data = filter(shots_plot, shot_result == "Goal"),
    x = ~x_plot,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    name = "Goal",
    text = ~hover_text,
    hovertemplate = "%{text}<extra></extra>",
    marker = list(
      color = "#F4C542",
      size = 6,
      opacity = 0.95,
      line = list(color = "white", width = 0.8)
    )
  )

# Pitch spots
fig <- fig %>%
  add_trace(
    x = c(60, 12, 108),
    y = c(40, 40, 40),
    type = "scatter",
    mode = "markers",
    marker = list(color = "white", size = 5),
    hoverinfo = "skip",
    showlegend = FALSE
  )

# --------------------------------------------------
# 4) Add pitch lines and layout
# --------------------------------------------------
fig <- fig %>%
  layout(
    title = list(
      text = "<b>Cristiano Ronaldo Shot Map</b><br><sup>Hover over a shot for details</sup>",
      font = list(color = "white", size = 22),
      x = 0.5
    ),
    
    paper_bgcolor = "#0B5D1E",
    plot_bgcolor = "#0B5D1E",
    font = list(color = "white"),
    
    xaxis = list(
      range = c(-2, 122),
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE,
      fixedrange = TRUE,
      title = ""
    ),
    
    yaxis = list(
      range = c(0, 80),
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE,
      fixedrange = TRUE,
      scaleanchor = "x",
      scaleratio = 1,
      title = ""
    ),
    
    legend = list(
      title = list(text = ""),
      bgcolor = "rgba(0,0,0,0.20)",
      font = list(color = "white", size = 12),
      x = 0.02,
      y = 0.98
    ),
    
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black", size = 12)
    ),
    
    shapes = list(
      # Outer boundary
      list(type = "rect", x0 = 0, x1 = 120, y0 = 0, y1 = 80,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Halfway line
      list(type = "line", x0 = 60, x1 = 60, y0 = 0, y1 = 80,
           line = list(color = "white", width = 2)),
      
      # Center circle
      list(type = "circle", x0 = 50, x1 = 70, y0 = 30, y1 = 50,
           line = list(color = "white", width = 2)),
      
      # Left penalty box
      list(type = "rect", x0 = 0, x1 = 18, y0 = 18, y1 = 62,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Right penalty box
      list(type = "rect", x0 = 102, x1 = 120, y0 = 18, y1 = 62,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Left six-yard box
      list(type = "rect", x0 = 0, x1 = 6, y0 = 30, y1 = 50,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Right six-yard box
      list(type = "rect", x0 = 114, x1 = 120, y0 = 30, y1 = 50,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Left goal
      list(type = "rect", x0 = -2, x1 = 0, y0 = 36, y1 = 44,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)"),
      
      # Right goal
      list(type = "rect", x0 = 120, x1 = 122, y0 = 36, y1 = 44,
           line = list(color = "white", width = 2), fillcolor = "rgba(0,0,0,0)")
    )
  ) %>%
  config(displayModeBar = TRUE)

fig

