controlled_actions <- c(1:3, 13:16, 42)
own_errors <- c(9, 50, 51, 61)
defensive_actions <- c(7:8, 10:12, 41, 44, 49, 52, 54, 56, 59, 74)
stoppages_in_play <- c(4:6, 17:20, 22:23, 25, 27, 30, 57)
shots <- 13:16
not_in_seq <- c(17:25, 27:28, 30, 32, 34:40, 47, 63:68, 70:71, 75:77)

seq_break <- c(own_errors, defensive_actions, stoppages_in_play, shots)

data <- data %>% 
  mutate(controlled = case_when(type_id %in% controlled_actions & team_id == home_team_id ~ 'home', 
                                type_id %in% controlled_actions & team_id == away_team_id ~ 'away'),
         seq_break = type_id %in% seq_break)

data <- data %>% 
  rowid_to_column() %>%
  # To include shots in a sequence:
  mutate(seq_break2 = ifelse(seq_break & !is.na(controlled), FALSE, seq_break)) %>%
  group_by(game_id) %>% 
  mutate(ID = data.table::rleid(controlled, seq_break2)) %>%
  group_by(game_id, controlled, seq_break2, ID) %>%
  mutate(temp = row_number()) %>% 
  filter(!(is.na(controlled) & seq_break2 & row_number() > 1)) %>%
  ungroup() %>%
  group_by(game_id) %>% 
  mutate(ID2 = cumsum(seq_break2)) %>%
  drop_na(controlled) %>%
  mutate(seq = data.table::rleid(controlled, ID2)) %>%
  ungroup() %>% 
  select(rowid, seq_break2, seq) %>%
  left_join(rowid_to_column(data), ., by = "rowid") %>%
  select(-rowid, -seq_break, -seq_break2)    
  
data %>% 
  group_by(game_id, seq) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  filter(!is.na(seq)) %>% 
  ggplot() + geom_bar(aes(n)) + scale_y_log10()

seq_info <- data %>%
  arrange(game_id, period_id, min, sec, timestamp) %>% 
  group_by(game_id, seq) %>% 
  summarise(seq_n = n(),
            seq_time = difftime(last(timestamp), first(timestamp), unit = 'secs') %>% as.numeric(),
            seq_time = ifelse(seq_time < 0, 
                              last(min) * 60 + last(sec) - first(min) * 60 - first(sec), 
                              seq_time),
            seq_progress = last(x) - first(x),
            seq_direct_speed = ifelse(seq_time > 0, seq_progress / seq_time, NA)) %>% 
  ungroup() %>% 
  filter(!is.na(seq))

seq_info %>% 
  select(starts_with('seq_')) %>% 
  summary()

# There are some missing and unreal values in seq_direct_speed - let's inspect them.
seq_info %>% 
  filter(is.na(seq_direct_speed)) %>% 
  group_by(seq_n) %>% 
  count(sort = TRUE)
# All NAs but one refers to sequences of length 1 - we'll replace those by 0 and - for modelling 
# purposes - add a new binary variable 'assisted' assigning 0 for all of these

seq_info %>% 
  filter(is.na(seq_direct_speed), seq_n > 1)
# For this sequence of two events we can doubtlessly assume that it didn't happen in a timespan of 0.
# Let's assign 1 to seq_time and recalculate seq_direct_speed.

seq_info <- seq_info %>% 
  mutate(seq_time = ifelse(is.na(seq_direct_speed) & seq_n > 1, 1, seq_time),
         seq_direct_speed = ifelse(seq_time > 0, seq_progress / seq_time, NA))

seq_info %>% ggplot() + geom_histogram(aes(seq_time))
seq_info %>% ggplot() + geom_histogram(aes(seq_progress))
seq_info %>% ggplot() + geom_histogram(aes(seq_direct_speed))

quantile0.05 <- quantile(seq_info$seq_direct_speed, prob = .05, na.rm = TRUE)
quantile0.95 <- quantile(seq_info$seq_direct_speed, prob = .95, na.rm = TRUE)
# The 0.05 and 0.95 quantiles' values seems pretty reasonable, so we'll winsorize seq_direct_speed 
# using them.
seq_info <- seq_info %>% 
  mutate(seq_direct_speed = ifelse(seq_direct_speed > quantile0.95, quantile0.95, seq_direct_speed),
         seq_direct_speed = ifelse(seq_direct_speed < quantile0.05, quantile0.05, seq_direct_speed))

# Lastly, we replace NAs with 0 as explained earlier.  
seq_info <- seq_info %>% 
  replace_na(list(seq_direct_speed = 0))

data <- data %>% 
  left_join(seq_info, by = c('game_id', 'seq'))

# Finally, we'll add auxiliary variable called 'assisted' for all events that were preceded by pass.  
# As one-length sequences couldn't be assisted, this will also set-off the effect of all 0's that 
# replaced NAs in seq_direct_speed.

data <- data %>% 
  group_by(game_id, seq) %>% 
  mutate(assisted = lag(type_id, default = 0) == 1) %>% 
  ungroup()

