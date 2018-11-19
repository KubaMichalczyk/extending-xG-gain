filter_with_context <- function(data, ..., before = 1, after = 1){
  conds <- enquos(...)
  
  rowids <- data %>% 
    rowid_to_column() %>% 
    filter(!!! conds) %>% 
    select(rowid) %>% 
    unlist()
  
  rowids <- c(outer(rowids, seq(from = -before, to = after), `+`)) %>% 
    unique() %>% 
    sort()
  rowids <- rowids[which(rowids >= 1 & rowids <= nrow(data))]
  
  
  data %>%
    filter(row_number() %in% rowids)
}

calc_time_played <- function(.game_id){
  print(.game_id)
  
  single_game <- data %>% 
    filter(game_id == .game_id)
  
  starting_line_ups <- single_game %>% 
    filter(type_id == 34) %>% 
    select(game_id, team_id, player_id, type_id, outcome, min, sec, q30) %>% 
    separate(q30, paste('player', 1:11, sep = '_'), sep = ', ') %>% 
    select(game_id, team_id, starts_with('player_'), min, sec) %>%
    select(-player_id) %>% 
    gather(key, player_id, -team_id, -min, -sec, -game_id) %>% 
    select(game_id, team_id, player_id, min_in = min, sec_in = sec) %>% 
    mutate(player_id = parse_integer(player_id))
  
  player_in <- single_game %>% 
    filter(type_id == 19) %>% 
    select(game_id, team_id, player_id, min_in = min, sec_in = sec)
  
  player_out <- single_game %>% 
    filter(type_id %in% c(18, 20)) %>% 
    select(game_id, team_id, player_id, min_out = min, sec_out = sec)
  
  game_length <- single_game %>% 
    filter(type_id == 30, period_id == 2) %>% 
    mutate(game_length = lubridate::duration(minute = min, second = sec)) %>% 
    group_by(game_id) %>% 
    summarise(game_length = max(game_length))
  
  time_played <- bind_rows(starting_line_ups, player_in) %>% 
    left_join(player_out, by = c('game_id', 'team_id', 'player_id')) %>% 
    left_join(game_length, by = c('game_id')) %>% 
    mutate(min_out = ifelse(is.na(min_out), game_length %/% 60, min_out),
           sec_out = ifelse(is.na(sec_out), game_length %% 60, sec_out),
           time_played = lubridate::duration(min = min_out - min_in, 
                                  sec = sec_out - sec_in)) %>% 
    select(game_id, player_id, time_played)
  
  return(time_played)
}
