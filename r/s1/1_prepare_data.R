### Script to prepare the data of study 1

library(tidyverse)

### Load data ==================================================================
posterior <- read_csv("data/s1/bart_posterior.csv")
bart <- read_csv("data/s1/bart_results.csv")
bartassess <- read_csv("data/s1/bartassess.csv")
participants <- read_csv("data/s1/participants.csv")
grips <- read_csv("data/s1/grips_results.csv")
soep <- read_csv("data/s1/soep_results.csv")
rlrisk <- read_csv("data/s1/rlrisk_results.csv")
demographics <- read_csv("data/s1/demographics.csv")

### Preprocess data ============================================================

# compute adjusted BART score, bart score, and number of explosions
bart <- bart %>%
  distinct(partid, trial, .keep_all = TRUE) %>%
  group_by(partid) %>%
  summarise(
    adj_bart_score = mean(number_of_clicks[exploded == 0]),
    adj_bart_score_early = mean(number_of_clicks[exploded == 0 & trial <= 10]),
    adj_bart_score_middle = mean(number_of_clicks[exploded == 0  & trial > 10 & trial <= 20]),
    adj_bart_score_late = mean(number_of_clicks[exploded == 0  & trial >= 21]),
    bart_score = mean(number_of_clicks),
    bart_score_early = mean(number_of_clicks[trial <= 10]),
    bart_score_middle = mean(number_of_clicks[trial > 10 & trial <= 20]),
    bart_score_late = mean(number_of_clicks[trial >= 21]),
    n_explosions = sum(exploded)
  ) %>%
  ungroup()

# compute grips scores
grips <- grips %>%
  select(partid:rating) %>%
  distinct(partid, quest_label, .keep_all = TRUE) %>%
  group_by(partid) %>%
  summarise(
    grips_score = mean(rating),
    n_grips = n()
  )

# reformat soep data
soep <- soep %>%
  select(partid:rating) %>%
  distinct(partid, quest_label, .keep_all = TRUE) %>%
  pivot_wider(names_from = quest_label,
              values_from = rating)

# reformat real-life risk taking data
rlrisk <- rlrisk %>%
  mutate(rating2_n = case_when(rating2 == "day" ~ 1,
                               rating2 == "week" ~ 7,
                               rating2 == "month" ~ 30.5,
                               rating2 == "year" ~ 365),
         times_per_day = rating1 / rating2_n) %>%
  select(partid, quest_label, times_per_day) %>%
  distinct(partid, quest_label, .keep_all = TRUE) %>%
  pivot_wider(names_from = quest_label,
              values_from = times_per_day) %>%
  mutate(rlrisk_gamble_b = case_when(rlrisk_gamble == 0 ~ 0,
                                     TRUE ~ 1),
         rlrisk_sport_b = case_when(rlrisk_sport == 0 ~ 0,
                                     TRUE ~ 1),
         rlrisk_cigarettes_b = case_when(rlrisk_cigarettes == 0 ~ 0,
                                     TRUE ~ 1),
         rlrisk_invest_b = case_when(rlrisk_invest == 0 ~ 0,
                                     TRUE ~ 1),
         rlrisk_speed_b = case_when(rlrisk_speed == 0 ~ 0,
                                     TRUE ~ 1),
         rlrisk_drink_b = case_when(rlrisk_drink == 0 ~ 0,
                                     TRUE ~ 1))

# merge data frames
dat <- participants %>%
  filter(imc >= 1 & device %in% c("desktop", "laptop") & qual_focused >= 25 &
           done == 1) %>%
  select(-id, -session, -imcrl, -imcgrips, -imc, -done, -qual_focused,
         -current_task, -start, -end) %>%
  distinct(partid, .keep_all = TRUE) %>%
  mutate(duration = duration / 60) %>%
  rename(bart_payoff = bonus) %>%
  left_join(bart, by = "partid") %>%
  left_join(posterior %>%
              distinct(partid, .keep_all = TRUE), by = "partid") %>%
  select(-id) %>%
  rename(posterior_belief = size,
         posterior_belief_time = time) %>%
  left_join(bartassess %>%
              distinct(partid, .keep_all = TRUE), by = "partid") %>%
  select(-id) %>%
  left_join(grips, by = "partid") %>%
  filter(n_grips == 8) %>%
  select(-n_grips) %>%
  left_join(soep, by = "partid") %>%
  left_join(rlrisk, by = "partid") %>%
  left_join(demographics, by = "partid") %>%
  select(-id) %>%
  drop_na() 

nrow(dat)

saveRDS(dat, "data/s1/study_1.RDS")
