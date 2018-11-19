passes <- data %>% 
  filter(type_id == 1, is.na(q2), is.na(q107)) # this includes keeper throws!

# Creating features ----
passes <- passes %>% 
  mutate(dist_start = calc_dist(x, y),
         angle_start = calc_angle(x, y),
         dist_end = calc_dist(q140, q141),
         angle_end = calc_angle(q140, q141))

passes <- passes %>% 
  mutate(xG_start = predict(model, newdata = select(., dist = dist_start, angle = angle_start, 
                                                    game_state, seq_direct_speed, assisted),
                            type = 'prob') %>% pluck('goal')) %>% 
  mutate(xG_end = predict(model, newdata = mutate(., assisted = TRUE) %>% 
                            select(dist = dist_end, angle = angle_end,
                                   game_state, seq_direct_speed, assisted),
                          type = 'prob') %>% pluck('goal'))

shots_full <- shots_full %>%
  mutate(xG = predict(model, newdata = select(., dist, angle, game_state, seq_direct_speed, assisted),
                      type = 'prob') %>% pluck('goal'))

passes %>%
  select(xG_start, xG_end) %>% 
  gather(type, value, xG_start, xG_end) %>% 
  ggplot() + geom_histogram(aes(value)) + facet_wrap(~type) + scale_y_log10()

transform_xG <- function(d, a, s) 1 - (log((1 + s)/ (d + s)) / log((1 + s) / s)) ^ a
  
# raw xG gain ----
passes %>% 
  filter(outcome == 1) %>%
  mutate(xG_gain = xG_end - xG_start) %>%
  group_by(player_id) %>%
  summarise(xG_gain_total = sum(xG_gain, na.rm = TRUE)) %>%
  right_join(player_info, by = 'player_id') %>%
  filter(position != 'Goalkeeper') %>%
  left_join(total_time_played, by = 'player_id') %>% 
  filter(time_played >= 1000 * 60) %>% 
  mutate(xG_gain_total_p90 = xG_gain_total / p90_adjustment) %>% 
  arrange(desc(xG_gain_total_p90)) %>%
  head(20) %>%
  ggplot() + geom_col(aes(reorder(name, xG_gain_total_p90), xG_gain_total_p90), fill = '#133E6B') + 
  coord_flip() +
  labs(x = NULL, y = 'xG gain p90') + 
  theme_minimal() + 
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B'))

# penalty area (a = 2 ^ 5, s = 2 ^ 15 * .1) ----
passes %>% 
  filter(outcome == 1) %>%
  mutate(xG_gain = transform_xG(xG_end, a = 2 ^ 5, s = 2 ^ 15 * .1) - 
           transform_xG(xG_start, a = 2 ^ 5, s = 2 ^ 15 * .1)) %>%
  group_by(player_id) %>%
  summarise(xG_gain_total = sum(xG_gain, na.rm = TRUE)) %>%
  right_join(player_info, by = 'player_id') %>%
  filter(position != 'Goalkeeper') %>%
  left_join(total_time_played, by = 'player_id') %>%
  filter(time_played >= 1000 * 60) %>% 
  mutate(xG_gain_total_p90 = xG_gain_total / p90_adjustment) %>% 
  arrange(desc(xG_gain_total_p90)) %>%
  head(20) %>%
  ggplot() + geom_col(aes(reorder(name, xG_gain_total_p90), xG_gain_total_p90), fill = '#133E6B') + 
  coord_flip() +
  labs(x = NULL, y = 'Transformed xG gain p90') + 
  theme_minimal() + 
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B'))

# Final third (a = 2 ^ 8, s = 2 ^ 15 * .1) ----
passes %>% 
  filter(outcome == 1) %>%
  mutate(xG_gain = transform_xG(xG_end, a = 2 ^ 8, s = 2 ^ 15 * .1) - 
           transform_xG(xG_start, a = 2 ^ 8, s = 2 ^ 15 * .1)) %>%
  group_by(player_id) %>%
  summarise(xG_gain_total = sum(xG_gain, na.rm = TRUE)) %>%
  right_join(player_info, by = 'player_id') %>%
  filter(position %in% c('Defender', 'Midfielder')) %>%
  left_join(total_time_played, by = 'player_id') %>%
  filter(time_played >= 1000 * 60) %>% 
  mutate(xG_gain_total_p90 = xG_gain_total / p90_adjustment) %>% 
  group_by(position) %>% 
  top_n(10, xG_gain_total_p90) %>%
  ggplot() + geom_col(aes(reorder(name, xG_gain_total_p90), xG_gain_total_p90), fill = '#133E6B') + 
  facet_wrap(~position, scales = 'free_y') + 
  coord_flip() +
  labs(x = NULL, y = 'Transformed xG gain p90') + 
  theme_minimal() + 
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B'))

# Final third (a = 2 ^ 11, s = 2 ^ 15 * .1) ----
passes %>% 
  filter(outcome == 1) %>%
  mutate(xG_gain = transform_xG(xG_end, a = 2 ^ 11, s = 2 ^ 15 * .1) - 
           transform_xG(xG_start, a = 2 ^ 11, s = 2 ^ 15 * .1)) %>%
  group_by(player_id) %>%
  summarise(xG_gain_total = sum(xG_gain, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(player_info, by = 'player_id') %>%
  filter(position %in% c('Defender', 'Midfielder')) %>%
  left_join(total_time_played, by = 'player_id') %>%
  filter(time_played >= 1000 * 60) %>% 
  mutate(xG_gain_total_p90 = xG_gain_total / p90_adjustment) %>% 
  group_by(position) %>% 
  top_n(10, xG_gain_total_p90) %>%
  ungroup() %>% 
  ggplot() + geom_col(aes(reorder(name, xG_gain_total_p90), xG_gain_total_p90), fill = '#133E6B') + 
  facet_wrap(~position, scales = 'free_y') + 
  coord_flip() +
  labs(x = NULL, y = 'Transformed xG gain p90') + 
  theme_minimal() + 
  theme(strip.text = element_text(color = '#4F5F6B', face = 'bold', size = 10),
        axis.title.x = element_text(color = '#4F5F6B', face = 'bold'),
        axis.title.y = element_text(color = '#4F5F6B', face = 'bold'),
        axis.text.x = element_text(color = '#4F5F6B'),
        axis.text.y = element_text(color = '#4F5F6B'))
