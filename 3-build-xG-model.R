library(caret)
library(rms)

shots_full <- data %>% 
  filter(type_id %in% 13:16 & is.na(q9) & is.na(q28)) %>% # this excludes penalties and own goals
  mutate(goal = factor(as.integer(type_id == 16)))

shots <- shots_full %>% 
  select(goal, dist, angle, game_state, seq_direct_speed, assisted)

# As we want a model that extrapolates well from typical shots locations to all passes locations,
# logistic regression will probably be the best choice. Due to small sample size we won't employ 
# standard training/test set split. Using stratified cross-validation instead will allow us to 
# make use of every point in a data set to build a model and estimate the true error rate on the side.

set.seed(0)
folds <- 10
cv_index <- createFolds(shots$goal, folds, returnTrain = TRUE)
ctrl <- trainControl(index = cv_index, method = 'cv', number = folds, classProbs = TRUE,
                     summaryFunction = twoClassSummary) 
model <- train(goal ~ ., data = shots %>% 
                 mutate(goal = fct_recode(goal, goal = '1', no_goal = '0')),
               method = 'glm', family = 'binomial', trControl = ctrl, metric = 'ROC')
# Fitted model has AUC = 0.769.

learning_curve <- data.frame(m = integer(21), 
                             train_error = integer(21),
                             cv_error = integer(21))

set.seed(0)
learning_curve <- learing_curve_dat(dat = as.data.frame(shots) %>%  
                                      mutate(goal = fct_recode(goal, goal = '1', no_goal = '0')), 
                                    outcome = 'goal',
                                    proportion = seq(1, 100, by = 3) / 100,
                                    method = 'glm', family = 'binomial',
                                    metric = 'ROC', 
                                    trControl = ctrl)

ggplot(learning_curve, aes(Training_Size, 1 - ROC, color = Data)) + 
  geom_smooth(method = 'loess', span = 1)
# We seems to encounter high bias.

summary(model)

# Angle is strongly not significant, which seems to be rather odd. Let's search for the true form of 
# angle variable.
logit_loess <- function(x, y, span){
  
  logit <- function(p) log(p/(1-p))
  
  loess_fit <- predict(loess(y ~ x, span = span))
  pi <- pmax(pmin(loess_fit,0.9999),0.0001)
  logit_fitted <- logit(pi)
  
  plt <- ggplot() + geom_point(aes(x, logit_fitted)) + scale_y_continuous(name = 'log-odds')
  return(plt)
}

logit_loess(shots$angle, as.numeric(as.character(shots$goal)), span = 0.1)
logit_loess(shots$angle, as.numeric(as.character(shots$goal)), span = 0.2)
logit_loess(shots$angle, as.numeric(as.character(shots$goal)), span = 0.3)
logit_loess(shots$angle, as.numeric(as.character(shots$goal)), span = 0.4)
# Plots don't suggest any clear form of angle. Let's check if a transformation with restricted 
# cubic splines can help.

list('3 knots' = 3, '4 knots' = 4, '5 knots' = 5, '6 knots' = 6) %>% 
  map(~glm(goal ~ rcs(angle, parms = .x), family = 'binomial', data = shots) %>% 
        summary() %>% pluck('aic'))
# Transformation with 4 knots results in lowest AIC - let's check if it helps our model.

model2 <- train(goal ~ . - angle + rcs(angle, parms = 4), data = shots %>% 
                 mutate(goal = fct_recode(goal, goal = '1', no_goal = '0')),
               method = 'glm', family = 'binomial', trControl = ctrl, metric = 'ROC')
summary(model2)
# All components of (transformed with rcs) angle are still statistically insignificant and the 
# updated model results in higher AIC - it seems that using rcs doesn't provide any value to our 
# model. 

shots_full %>% 
  filter(goal == 1) %>% 
  ggplot() + 
  ggsoccer::annotate_pitch() +
  geom_point(aes(x, y, color = angle, alpha = 0.5))
# A plot partly explains why angle can be seen as insignificant by our model - even if angle is far 
# from 1 (which is kind of perfect value) we can still observe a decent amount of goals. 
# Theoretically, we should consider removing angle from our model, but:
# - intuition strongly suggests that angle is important factor and I'm rather reluctant to go against
# it based on such a small sample
# - our model seems to already be underfitted - removing predictor certainly won't help (even if 
# a predictor doesn't provide much value itself)

# Note: Specifying angle as an angle between two rays connecting shot location and both goal post
# could improve our xG model, but doesn't generalise well as a pass characteristic as it would 
# result in tiny values for all locations far from goal.

pred <- predict(model, newdata = shots, type = 'prob')$goal

rmse <- function(x, y) sqrt(mean((x - y)^2))
rmse_all <- map(1:100, ~ shots %>%
                  mutate(xG = pred) %>% 
                  transmute(!! paste0("pred", .x) := as.numeric(xG > 0.01 * .x))) %>% 
  bind_cols(.) %>% map_dbl(rmse, y = as.numeric(as.character(shots$goal)))
ggplot() + geom_line(aes(seq(0.01, 1, by = 0.01), rmse_all))
rmse_all[which.min(rmse_all)]

# Both AUC and RMSE have values comparable to the models already published. So we'll leave it as it 
# is, keeping in mind that our main focus is to have a model with features that can be used also to 
# characterise passes, rather than building a model with most predictive power.


sd_vars <- shots %>% 
  select(-goal) %>% 
  map_dbl(sd)
beta <- summary(model)$coefficient[, 'Estimate'][-1] 
var_importance <- beta * sd_vars / (pi / sqrt(3))
var_importance %>% 
  data.frame() %>%
  rownames_to_column('var') %>%
  rename(std_coeff = '.') %>% 
  arrange(desc(abs(std_coeff))) %>% 
  ggplot() + geom_col(aes(reorder(var, abs(std_coeff)), abs(std_coeff))) + 
  coord_flip() +
  labs(x = 'Standardised coefficient', y = 'Variable') +
  theme_minimal()
