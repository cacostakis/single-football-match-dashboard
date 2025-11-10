---
  title: "Sample match dashboard"
format:
  dashboard:
  logo: "img/R_logo.png"
orientation: columns
---
  
  ```{r, loadpackages}
#| context: setup
library(shiny)
library(bslib)
library(tidyverse)
library(jsonlite)
library(plotly)
library(quarto)
```

```{r, setupdata}

# Load your data here

# Base URL
base <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data/"

# 1. Get available competitions
competitions <- fromJSON(paste0(base, "competitions.json"))

# 2. Get matches for a specific competition
# Example: competition_id = 43 (FIFA World Cup), season_id = 106 (2022)
comp_id <- 44 # MLS
season_id <- 107 # 2023
matches <- fromJSON(paste0(base, "matches/", comp_id, "/", season_id, ".json"))

# 3. Get events for a specific match
match_id <- matches$match_id[1]  # First match
events <- fromJSON(paste0(base, "events/", match_id, ".json"))

# 4. Get lineups
lineups <- fromJSON(paste0(base, "lineups/", match_id, ".json"))

# 5. Get StatsBomb 360 data (if available)
# Check if 360 data exists for this match
three_sixty_url <- paste0(base, "three-sixty/", match_id, ".json")


# Pitch dimensions   (CAN I AUTOMATE THESE EVENTUALLY)
pitch_L = 120
pitch_W = 80


# Events Df mutation
events_loc <-
  events %>%
  unnest(cols = c(pass, carry, dribble, shot, interception, duel, foul_committed, foul_won, ball_recovery, goalkeeper),
         names_sep = "_") %>%
  mutate(
    loc_L = map_dbl(location, ~if(length(.x) == 0) NA_real_ else .x[1]),
    loc_W = map_dbl(location, ~if(length(.x) == 0) NA_real_ else pitch_W-.x[2]),
    across(.cols = contains("end_location"),
           .fns = ~map_dbl(.x, ~if(length(.x) == 0) NA_real_ else .x[1]),
           .names = "{.col}_L"),
    across(.cols = contains("end_location"),
           .fns = ~map_dbl(.x, ~if(length(.x) == 0) NA_real_ else pitch_W-.x[2]),
           .names = "{.col}_W"),
    across(.cols = contains("end_location"),
           .fns = ~map_dbl(.x, ~if(length(.x) == 3) .x[3] else NA_real_),
           .names = "{.col}_H"),
    position_group = case_when(str_detect(position$name, "Back")~"Defender",
                               str_detect(position$name, "Mid")~"Midfielder",
                               str_detect(position$name, "Forward")~"Attacker",
                               str_detect(position$name, "Wing")~"Attacker",
                               str_detect(position$name, "Goal")~"Goalkeeper",
                               TRUE ~ "unknown"),
    period = case_when(
      period == 1 ~ "First half",
      period == 2 ~ "Second half",
      TRUE~"999"),
    pass_outcome_name = case_when(
      type$name == "Pass" & is.na(pass_outcome$name) ~ "Complete",
      TRUE ~ pass_outcome$name
    ),
    pass_outcome_name = 
      factor(pass_outcome_name, 
             levels = c("Complete", "Incomplete", "Out", "Pass Offside", "Unknown"))
  )
```

```{r, helper_functions}

plot_pitch_positions <- function(data = NULL, length = pitch_L, width = pitch_W) {
  
  # Standard proportions
  standard_length <- 105
  standard_width <- 68
  
  # Scale factors
  length_scale <- length / standard_length
  width_scale <- width / standard_width
  
  # Fixed dimensions scaled proportionally
  penalty_box_length <- 16.5 * length_scale
  penalty_box_width <- 40.32 * width_scale
  goal_area_length <- 5.5 * length_scale
  goal_area_width <- 18.32 * width_scale
  center_circle_radius <- 9.15 * min(length_scale, width_scale)
  penalty_spot_distance <- 11 * length_scale
  
  # Calculate positions
  half_length <- length / 2
  half_width <- width / 2
  
  penalty_box_y_min <- (width - penalty_box_width) / 2
  penalty_box_y_max <- (width + penalty_box_width) / 2
  
  goal_area_y_min <- (width - goal_area_width) / 2
  goal_area_y_max <- (width + goal_area_width) / 2
  
  # Create center circle data
  circle_data <- data.frame(
    theta = seq(0, 2*pi, length.out = 100)
  )
  circle_data$x <- half_length + center_circle_radius * cos(circle_data$theta)
  circle_data$y <- half_width + center_circle_radius * sin(circle_data$theta)
  
  # Initialize ggplot with data (if provided)
  p <- ggplot(data = data)
  
  # Add pitch elements as annotations
  p <- p +
    annotate("rect", xmin = 0, xmax = length, ymin = 0, ymax = width,
             fill = "seagreen", color = "white", linewidth = 1) +
    annotate("segment", x = half_length, xend = half_length,
             y = 0, yend = width,
             color = "white", linewidth = 1) +
    
    # Center circle using geom_path with its own data
    geom_path(data = circle_data,
              mapping = aes(x = x, y = y),
              color = "white", linewidth = 1,
              inherit.aes = FALSE) +
    
    annotate("point", x = half_length, y = half_width,
             color = "white", size = 2) +
    annotate("rect", xmin = 0, xmax = penalty_box_length,
             ymin = penalty_box_y_min, ymax = penalty_box_y_max,
             fill = NA, color = "white", linewidth = 1) +
    annotate("rect", xmin = length - penalty_box_length, xmax = length,
             ymin = penalty_box_y_min, ymax = penalty_box_y_max,
             fill = NA, color = "white", linewidth = 1) +
    annotate("rect", xmin = 0, xmax = goal_area_length,
             ymin = goal_area_y_min, ymax = goal_area_y_max,
             fill = NA, color = "white", linewidth = 1) +
    annotate("rect", xmin = length - goal_area_length, xmax = length,
             ymin = goal_area_y_min, ymax = goal_area_y_max,
             fill = NA, color = "white", linewidth = 1) +
    annotate("point", x = penalty_spot_distance, y = half_width,
             color = "white", size = 2) +
    annotate("point", x = length - penalty_spot_distance, y = half_width,
             color = "white", size = 2) +
    coord_fixed() +
    theme_void() +
    theme(
      panel.grid = element_blank(),        # Remove grid lines
      panel.border = element_blank(),      # Remove panel borders
      strip.background = element_blank(),  # Clean facet labels
      plot.background = element_blank()    # Remove plot background
    )
  
  return(p)
}
```

# Passing

## {.sidebar}

```{r tab_1_sidebar}
checkboxGroupInput("input_team", 
                   label = "Team:", 
                   choices = lineups$team_name, 
                   selected = matches$home_team$home_team_name[matches$match_id==match_id])

checkboxGroupInput("input_team", 
                   label = "Period of match:", 
                   choices = unique(events_loc$period), 
                   selected = unique(events_loc$period)
)
```

## Column

### Section 1 {height=25%}

```{r}
p(matches$competition$competition_name[matches$match_id==match_id])


```

### Section 2

```{r}
#| echo: false
#| include: false

ojs_define(event_df = events_loc)

```

```{ojs, pass_plot}
//| expandable: false

//| Transpose the data to make it usable in ojs

data = transpose(event_df)

```

### Section 3 {height=15%}

Data provided by [**StatsBomb open-data repository.**](https://github.com/statsbomb/open-data)
![StatsBomb logo image](img/statsbomb_logo.png){width="300px"}



# Tab 2

## {.sidebar}

```{r}
# More filters
```

## Column

```{r}
plotOutput("plot2")
```

# Tab 3

## Column

```{r}
plotOutput("plot3")
```

# Tab 4

## Column

```{r}
tableOutput("table2")
```



