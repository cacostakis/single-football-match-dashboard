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

pitch_L = 120
pitch_W = 80

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
                               TRUE ~ "unknown")
  )






# Pass specific mutations
pass <-
  events %>%
  filter(type$name == "Pass") %>%
  unnest(cols = pass, names_sep = "_") %>%
  mutate(
    loc_end_L = map_dbl(pass_end_location, ~if(length(.x) == 0) NA_real_ else .x[1]),
    loc_end_W = map_dbl(pass_end_location, ~if(length(.x) == 0) NA_real_ else pitch_W-.x[2]),
    pass_outcome_name = if_else(is.na(pass_outcome$name),"Complete",pass_outcome$name)
  )







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
    theme(plot.background = element_rect(fill = "lightgreen"))
  
  return(p)
}


# SOLID PASS CHART
pass %>%
  plot_pitch_positions(data=.) +
  geom_segment(aes(x = loc_L, y = loc_W, color = pass_outcome_name,
                   xend = loc_end_L, yend = loc_end_W),
               arrow = arrow(length = unit(0.8, "mm"), type = "closed")) +
  geom_point(aes(x= loc_L,
                 y= loc_W,
                 color = pass_outcome_name,
                 shape = pass_outcome_name),
             size = 0.8) +
  facet_grid(cols = vars(period),
             rows = vars(team$name)) +
  scale_color_manual(values = c("green", "red", "gold", "purple", "pink")) +
  scale_shape_manual(values = c(1, 4, 4, 4, 2))


# Pass  -- Single possession
pass %>%
  filter(possession==2) %>%
  plot_pitch_positions(data=.) +
  geom_segment(
    aes(x = pass_L, y = pass_W, color = pass_outcome_name,
        xend = pass_end_L, yend = pass_end_W),
    arrow = arrow(length = unit(0.8, "mm"), type = "closed")) +
  facet_grid(rows = vars(period), cols = vars(team_name)) +
  scale_color_manual(values = c("lightblue", "red", "gold", "purple", "pink"))




#__ SHOTS ___________________________________________________________________
shot <-
  events %>%
  filter(type$name == "Shot") %>%
  unnest(cols = c(shot), names_sep = "_") %>%
  # Pass specific mutations
  separate_wider_delim(cols = location, delim = ",", names = c("loc_L","loc_W"),
                       too_few = "align_start") %>%
  separate_wider_delim(cols = shot_end_location, delim = ",", names = c("loc_end_L","loc_end_W", "loc_end_H"),
                       too_few = "align_start") %>%
  mutate(across(contains("loc_"),
                .fns = ~str_extract(.x, "\\d+") %>% as.numeric))
across(c(loc_W, loc_end_W),
       .fns = ~(pitch_W-.x) %>% as.numeric),
shot_outcome_simple = case_when(shot_outcome$name == "Goal" ~ "Goal",
                                shot_outcome$name == "Off T" ~ "Off-target",
                                TRUE ~ "No goal"
)
shot %>% range(loc_L)
shot$loc_L
shot$loc_W
shot$shot_outcome
shot %>%
  plot_pitch_positions(data = .) +
  geom_point(aes(x= loc_L,
                 y= loc_W,
                 color = shot_outcome_simple),
             size = 0.8)  +
  facet_wrap(~team$name)
shot$team

# Trying to do possessions
pass %>%
  filter(possession == 111) %>%
  plot_pitch_positions(data=.) +
  geom_segment(aes(x = pass_L, y = pass_W, color = pass_outcome_name,
                   xend = pass_end_L, yend = pass_end_W),
               arrow = arrow(length = unit(0.8, "mm"), type = "closed")) +
  geom_point(aes(x= pass_L,
                 y= pass_W,
                 color = pass_outcome_name,
                 shape = pass_outcome_name),
             size = 0.8)
str(events)
events$shot
shot <-
  events %>%
  unnest(cols = shot, names_sep = "_")
str(shot)
events$type$name %>% table
t <-
  events %>%
  filter(type$name == "shot")
geom_point(data = .)
facet_grid(cols = vars(team_name),
           rows = vars(position_group)) +
  scale_color_manual(values = c("green", "red", "gold", "purple", "pink")) +
  scale_shape_manual(values = c(1, 4, 4, 4, 2))






events$shot$end_location[[2]]




