library(tidyverse)

data <- readRDS('D:/NYA Fellowship/raw-data/PL_2017-18.rds')
data <- data %>% 
  filter(!type_id == 43)

data <- data %>% 
  mutate(timestamp = lubridate::ymd_hms(timestamp))

# The following lines are to order the data. Exact ties are ordered so that the same values of team_id 
# are made adjacent.
data <- data %>% 
  arrange(game_id, period_id, min, sec, timestamp) %>% 
  mutate(group_id = group_indices(., game_id, period_id, min, sec, timestamp)) %>% 
  group_by(game_id) %>% 
  mutate(order = map2(
    split_ <- split(team_id, group_id),
    accumulate(split_, ~intersect(c(rev(.x), .y), .y)),
    match) %>% unlist) %>% 
  arrange(game_id, group_id, order) %>% 
  ungroup() %>% 
  select(-order, -group_id)

# Create new variables ----
data <- data %>% 
  group_by(game_id) %>%  
  mutate(q28_int = as.integer(levels(q28))[q28]) %>% 
  replace_na(list(q28_int = 0)) %>% 
  mutate(home_goal = (type_id == 16) * 
           (team_id == home_team_id & q28_int != 1 | team_id == away_team_id & q28_int == 1),
         away_goal = (type_id == 16) * 
           (team_id == away_team_id & q28_int != 1 | team_id == home_team_id & q28_int == 1),
         home_current_score = lag(cumsum(home_goal), default = 0),
         away_current_score = lag(cumsum(away_goal), default = 0),
         game_state = ifelse(team_id == home_team_id,
                             home_current_score - away_current_score,
                             away_current_score - home_current_score)) %>% 
  select(-home_current_score, -away_current_score) %>% 
  ungroup()

# Angle 1 for in front of goal, 0 for endline.
calc_dist <- function(x, y) sqrt((x - 100) ^ 2 + (y - 50) ^ 2)
calc_angle <- function(x, y) (abs(atan2(y - 50, x - 100)) - pi /2) / (pi / 2)
data <- data %>% 
  mutate(dist = calc_dist(x, y),
         angle = calc_angle(x, y))

player_info <- read_csv('D:/NYA Fellowship/raw-data/NYA-Opta-8-2017-playerinfo.csv') %>% 
  rename(player_id = player,
         position = pos)

team_info <- read_csv('D:/NYA Fellowship/raw-data/NYA-Opta-8-2017-teamnames.csv')

total_time_played <- data %>% 
  distinct(game_id) %>%
  unlist() %>% 
  map(calc_time_played) %>% 
  bind_rows() %>% 
  group_by(player_id) %>% 
  summarise(time_played = sum(time_played)) %>% 
  mutate(p90_adjustment = time_played / (90 * 60))

