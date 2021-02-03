### Script to prepare the data of study 1

library(tidyverse)

### Load data ==================================================================
posterior <- read_csv("data/s2/bart_posterior.csv")
bart <- read_csv("data/s2/bart_results.csv")
bartassess <- read_csv("data/s2/bartassess.csv")
participants <- read_csv("data/s2/participants.csv")
grips <- read_csv("data/s2/grips_results.csv")
soep <- read_csv("data/s2/soep_results.csv")
rlrisk <- read_csv("data/s2/rlrisk_results.csv")
upps <- read_csv("data/s2/upps_results.csv")

s1 <- readRDS("data/s1/study_1.RDS")
s1 <- s1 %>%
  select(partid, bart_payoff, adj_bart_score, bart_score, n_explosions,
         posterior_belief, distribution_rating, grips_score:education)

names(s1) <- c("partid", paste0(names(s1)[2:(ncol(s1) - 4)], "_s1"),
               names(s1)[(ncol(s1) - 3):ncol(s1)])

### Preprocess data ============================================================

# compute adjusted BART score, bart score, and number of explosions
bart <- bart %>%
  distinct(partid, trial, .keep_all = TRUE) %>%
  group_by(partid) %>%
  summarise(
    adj_bart_score = mean(number_of_clicks[exploded == 0]),
    bart_score = mean(number_of_clicks),
    n_explosions = sum(exploded)
  ) %>%
  ungroup()

bartassess <- bartassess %>%
  distinct(partid, .keep_all = TRUE) %>%
  mutate(distribution_rating = case_when(distribution_rating == "normal" ~ confidence_rating,
                                         TRUE ~ 0 - confidence_rating)) %>%
  select(partid, distribution_rating)

# compute grips scores
grips <- grips %>%
  select(partid:rating) %>%
  distinct(partid, quest_label, .keep_all = TRUE) %>%
  group_by(partid) %>%
  summarise(
    grips_score = mean(rating),
    n_grips = n()
  )

# compute upps scores
refl_ids <- c("urgency_11", "perseverance_2", "perseverance_10")
upps <- upps %>%
  select(partid:rating) %>%
  distinct(partid, quest_label, .keep_all = TRUE) %>%
  mutate(rating = case_when(quest_label %in% refl_ids ~ 5 - rating,
                            TRUE ~ rating),
         quest_label = gsub("_[[:digit:]]+", "", quest_label)) %>%
  group_by(partid, quest_label) %>%
  summarise(
    upps_score = mean(rating)
  ) %>%
  pivot_wider(values_from = upps_score, names_from = quest_label)

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
  left_join(bartassess) %>%
  left_join(grips, by = "partid") %>%
  filter(n_grips == 8) %>%
  select(-n_grips) %>%
  left_join(soep, by = "partid") %>%
  left_join(rlrisk, by = "partid") %>%
  left_join(upps, by = "partid") %>%
  drop_na() %>%
  left_join(s1, by = "partid")

nrow(dat)

saveRDS(dat, "data/s2/study_2.RDS")
