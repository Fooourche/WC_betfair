# +++++++
# Model: MakelELO
# Date: 23 / 5 / 2018
# +++++++

library(readr)
library(dplyr)
library(elo)
library(lubridate)
library(tidyverse)
library(h2o)
library(timetk)
library(tidyquant)

# Read in the world cup CSV data
training = read_csv("00_data/wc_datathon_dataset.csv")
rawdata$date = as.Date(rawdata$date)


# Lowercase team names
training$team_1 = tolower(training$team_1)
training$team_2 = tolower(training$team_2)

# Read in submission file
wc_2018 = read_csv("00_data/john_smith_numbersman1-1.csv")

# Fix the ELO k factor - here you can try different values to see if improves the model performance
k_fac = 20

# Run ELO
elo_run = elo.run(
  score(team_1_goals, team_2_goals) ~ team_1 + team_2,
  data = training,
  k = k_fac
)


training <- cbind(training,elo_run)

training <- training %>% 
              mutate(result=case_when((team_1_goals-team_2_goals) == 0 ~ 0,
                                      (team_1_goals-team_2_goals) < 0 ~ 2,
                                      (team_1_goals-team_2_goals) > 0 ~ 1))


training_aug <- training %>%
                tk_augment_timeseries_signature()



training_clean <- training_aug %>%
                    select_if(~ !is.Date(.)) %>%
                    select_if(~ !any(is.na(.))) %>%
                    mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

training_clean %>% glimpse()


train_tbl <- training_clean %>% filter(year < 2014)
valid_tbl <- training_clean %>% filter(year >= 2014 & year <= 2016)
test_tbl  <- training_clean %>% filter(year > 2016)

h2o.init()


train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)


# Set names for h2o
y <- "result"
x <- setdiff(names(train_h2o), y)


# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
                        x = x, 
                        y = y, 
                        training_frame = train_h2o, 
                        validation_frame = valid_h2o, 
                        leaderboard_frame = test_h2o, 
                        max_runtime_secs = 60, 
                        stopping_metric = "deviance")

# Extract leader model
automl_leader <- automl_models_h2o@leader


pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)


h2o.performance(automl_leader, newdata = test_h2o)

# Investigate test error
error_tbl <- training %>% 
  filter(lubridate::year(date) == 2017) %>%
  add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = result) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl


error_tbl %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) %>%
  glimpse()






# Draw Rates
draw_rates = data.frame(win_prob = elo_run$elos[,3],win_loss_draw = elo_run$elos[,4]) %>%
                mutate(prob_bucket = abs(round((win_prob-(1-win_prob))*20)) / 20) %>%
                group_by(prob_bucket) %>%
                summarise(draw_prob = sum(ifelse(win_loss_draw==0.5, 1, 0)) / n())

# Run predictions on 2018 world cup: the predict function, in this case, just needs the home and away team names for the tournament
wc_2018_home_probabilities = predict(elo_run, newdata = wc_2018 %>% select(team_1, team_2))

# To our WC 2018 dataset let's add in our predicted win probabilities and fold in the expected draw rates from our table above
wc_2018 = wc_2018 %>%
  select(-prob_team_1_draw) %>%
  mutate(
    prob_team_1_win = wc_2018_home_probabilities,
    prob_team_1_lose = 1 - prob_team_1_win,
    prob_bucket = round(20 * abs((prob_team_1_win - prob_team_1_lose))) / 20
  ) %>%
  left_join(draw_rates) %>%
  mutate(
    prob_team_1_win = prob_team_1_win - 0.5 * draw_prob,
    prob_team_1_lose = prob_team_1_lose - 0.5 * draw_prob
  ) %>%
  select(date, match_id, team_1, team_2, prob_team_1_win, "prob_team_1_draw" = draw_prob, prob_team_1_lose, -prob_bucket)

# Write submission to file
write_csv(wc_2018, "betfair_datascientists_makelelo.csv")
