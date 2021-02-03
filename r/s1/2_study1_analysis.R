### Analysis script study 1

library(tidyverse)
library(bayestestR)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(BEST)
library(BayesFactor)
library(insight)
library(ggrepel)
library(psych)
library(viridis)
library(see)
library(patchwork)
library(GGally)
library(xtable)
source("r/s1/functions.R")


s1 <- readRDS("data/s1/study_1.RDS")
bart <- read_csv("data/s1/bart_results.csv")
bart <- bart %>%
  distinct(partid, trial, .keep_all = TRUE) %>%
  filter(partid %in% unique(s1$partid)) %>%
  distinct(partid, trial, .keep_all = TRUE) %>%
  left_join(s1 %>% select(partid, cond_dist, distribution_rating), by = "partid")

dists <- readRDS("data/experiment/bart_distributions.RDS")


### Demographics ===============================================================

prop.table(table(s1$sex))

mean(s1$age)
sd(s1$age)

prop.table(table(s1$education))

prop.table(table(s1$job))

### Descriptive Statistics =====================================================

# completion time
mean(s1$duration)
#payoff
mean(s1$bart_payoff) + .1

s1 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  group_by(cond_dist) %>%
  summarise(
    m_adj_bart_score = mean(adj_bart_score),
    sd_adj_bart_score = sd(adj_bart_score),
    m_posterior_belief = mean(posterior_belief),
    sd_posterior_belief = sd(posterior_belief),
    m_confidence = mean(distribution_rating),
    sd_confidence = sd(distribution_rating),
    m_grips = mean(grips_score),
    sd_grips = sd(grips_score),
    m_soep_gen = mean(soep_gen),
    sd_soep_gen = sd(soep_gen),
    N = n()
  ) %>%
  View()

# for table in SM
s1 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  group_by(cond_dist) %>%
  summarise(
    adj_bart_score = paste0(round(mean(adj_bart_score), 2), " (",
                            round(sd(adj_bart_score), 2), ")"),
    posterior_belief = paste0(round(mean(posterior_belief), 2), " (",
                              round(sd(posterior_belief), 2), ")"),
    n_explosions = paste0(round(mean(n_explosions), 2), " (",
                            round(sd(n_explosions), 2), ")"),
    dist_form_belief = paste0(round(mean(distribution_rating), 2), " (",
                             round(sd(distribution_rating), 2), ")"),
    grips = paste0(round(mean(grips_score), 2), " (",
                   round(sd(grips_score), 2), ")"),
    soep_gen = paste0(round(mean(soep_gen), 2), " (",
                        round(sd(soep_gen), 2), ")"),
    soep_driving = paste0(round(mean(soep_driving), 2), " (",
                          round(sd(soep_driving), 2), ")"),
    soep_faith = paste0(round(mean(soep_faith), 2), " (",
                        round(sd(soep_faith), 2), ")"),
    soep_finance = paste0(round(mean(soep_finance), 2), " (",
                          round(sd(soep_finance), 2), ")"),
    soep_health = paste0(round(mean(soep_health), 2), " (",
                         round(sd(soep_health), 2), ")"),
    soep_leisure = paste0(round(mean(soep_leisure), 2), " (",
                      round(sd(soep_leisure), 2), ")"),
    soep_occupation = paste0(round(mean(soep_occupation), 2), " (",
                      round(sd(soep_occupation), 2), ")"),
    rlrisk_cigarettes_b = paste0(round(mean(rlrisk_cigarettes_b), 2)),
    rlrisk_drink_b = paste0(round(mean(rlrisk_drink_b), 2)),
    rlrisk_gamble_b = paste0(round(mean(rlrisk_gamble_b), 2)),
    rlrisk_invest_b = paste0(round(mean(rlrisk_invest_b), 2)),
    rlrisk_speed_b = paste0(round(mean(rlrisk_speed_b), 2)),
    rlrisk_sport_b = paste0(round(mean(rlrisk_sport_b), 2)),
    N = as.character(n())
  ) %>%
  ungroup() %>% 
  pivot_longer(cols = adj_bart_score:N, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = cond_dist) %>% 
  xtable() %>% print(include.rownames = FALSE)

# distributions of real-life risk measures
s1 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  pivot_longer(ends_with("_b"), names_to = "rlrisk", values_to = "freq") %>%
  ggplot(aes(freq)) +
  geom_histogram() +
  facet_grid(cond_dist ~ rlrisk)

s1 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  pivot_longer(starts_with("soep"), names_to = "soep", values_to = "rating") %>%
  ggplot(aes(rating)) +
  geom_histogram() +
  facet_grid(cond_dist ~ soep)


# correlations between different risk preference measures

pdf("plots/s1/cors_measures.pdf", height = 6, width = 7)
s1 %>%
  select(grips_score, starts_with("soep"),
         ends_with("_b")) %>%
  rename(GRiPS = grips_score,
         SOEPgeneral = soep_gen,
         SOEPdriving = soep_driving,
         SOEPfinance = soep_finance,
         SOEPhealth = soep_health,
         SOEPleisure = soep_leisure,
         SOEPoccupation = soep_occupation,
         SOEPsocial = soep_faith,
         Drinking = rlrisk_drink_b,
         Gambling = rlrisk_gamble_b,
         Investing = rlrisk_invest_b,
         Smoking = rlrisk_cigarettes_b,
         Speeding = rlrisk_speed_b,
         Sport = rlrisk_sport_b) %>%
  ggcorr(label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE,
         hjust = 0.9, method = c("pairwise", "spearman"),
         name = expression(italic(r[s])), palette = "PuOr", layout.exp = 1)
dev.off()

# learning in the BART across conditions

cols <- viridis(4, end = .8,  alpha = .8)
names(cols) <- c("Uniform", "Normal High SD", 
                 "Normal Moderate SD", "Normal Low SD")

cors_dists <- bart %>%
  filter(exploded == 0) %>%
  group_by(partid, cond_dist) %>%
  summarise(
    r_s = cor(trial, number_of_clicks, method = "spearman")
  )

ggplot(cors_dists, aes(r_s)) +
  geom_histogram() +
  facet_wrap(~ cond_dist)

cors_dists_agg <- cors_dists %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"),
                            labels = c("Uniform", "Normal High SD", 
                                       "Normal Moderate SD", "Normal Low SD"))) %>%
  group_by(cond_dist) %>%
  summarise(m_r = round(mean(r_s, na.rm = TRUE), 2)) 

pdf("plots/s1/bart_behavior.pdf", height = 6, width = 10)
bart %>%
  filter(exploded == 0) %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"),
                            labels = c("Uniform", "Normal High SD", 
                              "Normal Moderate SD", "Normal Low SD"))) %>%
  ggplot(aes(trial, number_of_clicks, col = cond_dist, group = partid)) +
  geom_point(alpha = .05) + 
  geom_line(alpha = .05) +
  stat_summary(fun.y = "mean", geom = "point", size = 2, group = 1, alpha = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1, alpha = 1) +
  geom_label(data = cors_dists_agg, aes(x = 3, y = 58, group = 1, col = cond_dist,
                                        label = paste0("italic(r[s])==", m_r)),
             parse = TRUE, size = 4) +
  scale_color_manual(values = cols) +
  facet_wrap(~ cond_dist) +
  theme_light() +
  labs(
    x = "Trial",
    y = "Number of Inflations (No Explosions)"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
dev.off()

### H1 =========================================================================

# H1: General task representation: At the end of the task, participants will
#     believe that the explosion points cluster around a mean value rather than that
#     they are randomly distributed, irrespective of the actual distributional form
#     implemented (i.e., BARTuniform vs. BARTnormal). Moreover, within BARTnormal,
#     we expect this belief to be increasingly stronger the smaller the standard
#     deviation of the distributions becomes.

H1_dat <- s1 %>%
  select(distribution_rating, cond_dist) %>%
  mutate(cond_dist = factor(cond_dist, levels = c("uniform", "normal_high",
                                                  "normal_moderate", "normal_low")))
H1_dat %>%
  group_by(cond_dist) %>%
  summarise(
    p_normal = sum(distribution_rating > 25) / (sum(distribution_rating > 25) +
                                                  sum(distribution_rating < 25)) * 100
  ) %>% pull(p_normal)


# means
H1_dat %>%
  group_by(cond_dist) %>%
  summarise(
    m_belief = mean(distribution_rating)
  ) %>% pull(m_belief)

# create contrasts
c1 <- c(-1 , 1/3, 1/3,  1/3)
c2 <- c( 0,    1,  -1,    0)
c3 <- c( 0,    0,   1,   -1)

# create temporary matrix
temp_cs <- rbind(constant=1/4, c1, c2, c3)

# get the inverse of that matrix
cs <- solve(temp_cs)

# drop the first column
cs <- cs[ , -1]

contrasts(H1_dat$cond_dist) <- cs

m_H1 <- stan_glm(distribution_rating ~ cond_dist, data = H1_dat,
                 chains = 4, iter = 10000, family = gaussian(),
                 prior_intercept = normal(location = 0, scale = 10),
                 prior = normal(location = 0, scale = 2.5))


describe_posterior(m_H1, test = c("rope"), ci = .95, rope_ci = 1)
# effect sizes
cohens_d(H1_dat$distribution_rating[H1_dat$cond_dist != "uniform"],
         H1_dat$distribution_rating[H1_dat$cond_dist == "uniform"])
cohens_d(H1_dat$distribution_rating[H1_dat$cond_dist == "normal_moderate"],
         H1_dat$distribution_rating[H1_dat$cond_dist == "normal_high"])
cohens_d(H1_dat$distribution_rating[H1_dat$cond_dist == "normal_low"],
         H1_dat$distribution_rating[H1_dat$cond_dist == "normal_moderate"])

# Plot the different distributions
pdf("plots/s1/H1_V2.pdf", height = 6, width = 10)
temp <- H1_dat %>%
  mutate(cond_dist = factor(cond_dist, levels = c("normal_low", "normal_moderate",
                                                  "normal_high", "uniform")))

cols <- viridis(4, end = .8,  alpha = .8)
names(cols) <- c("uniform", "normal_high",
                 "normal_moderate", "normal_low")
cols_m <- viridis(4, end = .8,  alpha = 1)
names(cols_m) <- c("uniform", "normal_high",
                 "normal_moderate", "normal_low")
ord <- c("normal_low", "normal_moderate",
         "normal_high", "uniform")

par(mar = c(4, 6.75, 0, .7))
plot.new()
plot.window(xlim = c(0, 50), ylim = c(1, 4.7))
abline(v = 25, col = "darkgrey", lty = 3, lwd = 2.5)
axis(1, at = seq(0, 50, 5), cex.axis = 1, lwd = 2)
text(c(expression(BART[normal-L]), expression(BART[normal-M]),
       expression(BART[normal-H]), expression(BART[uniform])),
     y = c(1.25, 2.25, 3.25, 4.25), x = -9.5, xpd = TRUE, offset = 0, adj = 0,
     cex = 1.25)
mtext("Belief About Distributional Form", side = 1, line = 3, cex = 1.25)
lines(rep(median(temp$distribution_rating[temp$cond_dist == "normal_low"]), 2),
      c(1, 1.7), lty = 1, lwd = 4.5, col = grey(0, .8))
lines(rep(median(temp$distribution_rating[temp$cond_dist == "normal_moderate"]), 2),
      c(2, 2.7), lty = 1, lwd = 4.5, col = grey(0, .8))
lines(rep(median(temp$distribution_rating[temp$cond_dist == "normal_high"]), 2),
      c(3, 3.7), lty = 1, lwd = 4.5, col = grey(0, .8))
lines(rep(median(temp$distribution_rating[temp$cond_dist == "uniform"]), 2),
      c(4, 4.7), lty = 1, lwd = 4.5, col = grey(0, .8))

stripchart(distribution_rating ~ cond_dist, data = temp, method = "stack",
           pch=16, col = cols[ord], add = TRUE) 

text(c("Uniform", "Normal"), y = rep(.43, 2), x = c(0, 50),  xpd = TRUE,
     offset = 0, adj = .5, cex = 1.25)

dev.off()
### H2 =========================================================================

# H2: Beliefs about optimal behavior: Participants' posterior beliefs about the
#     value that maximizes their payoffs will exhibit less variability between
#     participants in the BARTnormal as opposed to in the BARTuniform. Moreover,
#     these beliefs will be closer to the value that actually maximizes payoffs in
#     the former as compared to in the latter.

### estimate distributions --

mod_H2_unif <- s1 %>%
  filter(cond_dist == "uniform") %>%
  select(posterior_belief) %>%
  pull() %>%
  BESTmcmc()

mod_H2_normal_high <- s1 %>%
  filter(cond_dist == "normal_high") %>%
  select(posterior_belief) %>%
  pull() %>%
  BESTmcmc()

mod_H2_normal_moderate <- s1 %>%
  filter(cond_dist == "normal_moderate") %>%
  select(posterior_belief) %>%
  pull() %>%
  BESTmcmc()

mod_H2_normal_low <- s1 %>%
  filter(cond_dist == "normal_low") %>%
  select(posterior_belief) %>%
  pull() %>%
  BESTmcmc()

### compare distributions --

## describtive statistics of distributions --

describe_posterior(mod_H2_unif, ci = .95)
describe_posterior(mod_H2_normal_high, ci = .95)
describe_posterior(mod_H2_normal_moderate, ci = .95)
describe_posterior(mod_H2_normal_low, ci = .95)


## Uniform vs the three "Normal" levels: absolute differences in beliefs --

# Uniform vs. Normal High SD
compare_diffs(mod_H2_normal_high, mod_H2_unif)

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_high"],
         s1$posterior_belief[s1$cond_dist == "uniform"])

# Uniform vs. Normal Moderate SD
compare_diffs(mod_H2_normal_moderate, mod_H2_unif)

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_moderate"],
         s1$posterior_belief[s1$cond_dist == "uniform"])

# Uniform vs. Normal Low SD
compare_diffs(mod_H2_normal_low, mod_H2_unif)

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_low"],
         s1$posterior_belief[s1$cond_dist == "uniform"])

## Comparisons within the three "Normal" levels --

# Normal High SD vs. Normal Moderate SD
compare_diffs(mod_H2_normal_moderate, mod_H2_normal_high)

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_moderate"],
         s1$posterior_belief[s1$cond_dist == "normal_high"])

# Normal Moderate SD vs. Normal Low SD
compare_diffs(mod_H2_normal_low, mod_H2_normal_moderate)

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_low"],
         s1$posterior_belief[s1$cond_dist == "normal_moderate"])


## Uniform vs the three "Normal" levels: differences from optimal--

# Uniform vs. Normal High SD
diff_unif_norm_h_opt <- tibble(
  delta_m = (mod_H2_normal_high$mu - 28) - (mod_H2_unif$mu - 32)
)

describe_posterior(diff_unif_norm_h_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_h_opt$delta_m))

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_high"] - 28,
         s1$posterior_belief[s1$cond_dist == "uniform"] - 32)

# Uniform vs. Normal Moderate SD
diff_unif_norm_m_opt <- tibble(
  delta_m = (mod_H2_normal_moderate$mu - 25) - (mod_H2_unif$mu - 32)
)

describe_posterior(diff_unif_norm_m_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_m_opt$delta_m))

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_moderate"] - 25,
         s1$posterior_belief[s1$cond_dist == "uniform"] - 32)

# Uniform vs. Normal Low SD
diff_unif_norm_l_opt <- tibble(
  delta_m = (mod_H2_normal_low$mu - 25) - (mod_H2_unif$mu - 32)
)

describe_posterior(diff_unif_norm_l_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_l_opt$delta_m))

cohens_d(s1$posterior_belief[s1$cond_dist == "normal_low"] - 25,
         s1$posterior_belief[s1$cond_dist == "uniform"] - 32)

# Plot the different distributions
s1 %>%
  mutate(cond_dist = factor(cond_dist, levels = c("uniform", "normal_high",
                                                  "normal_moderate", "normal_low"),
                            labels = c("Uniform", "Normal High SD", 
                                    "Normal Moderate SD", "Normal Low SD"))) %>%
  ggplot(aes(cond_dist, posterior_belief)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .5) +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 2), 
               geom = "pointrange", color = "red", size = 1) +
  ylim(0, 65) +
  theme_bw() +
  labs(
    x = "Condition Levels",
    y = "Posterior Belief"
  )


### H3 =========================================================================

# H3: Overt risk-taking behavior: Participants' adjusted BART scores will, on
#     average, be closer to the optimal value and exhibit less variability between
#     participants in the BARTnormal than in the BARTuniform. Within the BARTnormal,
#     we expect that the adjusted BART scores will be increasingly closer to the
#     optimal value and exhibit less variability, the smaller the standard deviation
#     of the distribution becomes.

### estimate distributions --

mod_H3_unif <- s1 %>%
  filter(cond_dist == "uniform") %>%
  select(adj_bart_score) %>%
  pull() %>%
  BESTmcmc()

mod_H3_normal_high <- s1 %>%
  filter(cond_dist == "normal_high") %>%
  select(adj_bart_score) %>%
  pull() %>%
  BESTmcmc()

mod_H3_normal_moderate <- s1 %>%
  filter(cond_dist == "normal_moderate") %>%
  select(adj_bart_score) %>%
  pull() %>%
  BESTmcmc()

mod_H3_normal_low <- s1 %>%
  filter(cond_dist == "normal_low") %>%
  select(adj_bart_score) %>%
  pull() %>%
  BESTmcmc()

### compare distributions --

## describtive statistics of distributions --

describe_posterior(mod_H3_unif, ci = .95)
describe_posterior(mod_H3_normal_high, ci = .95)
describe_posterior(mod_H3_normal_moderate, ci = .95)
describe_posterior(mod_H3_normal_low, ci = .95)

## Uniform vs the three "Normal" levels --

# Uniform vs. Normal High SD
compare_diffs(mod_H3_normal_high, mod_H3_unif)

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_high"],
         s1$adj_bart_score[s1$cond_dist == "uniform"])

# Uniform vs. Normal Moderate SD
compare_diffs(mod_H3_normal_moderate, mod_H3_unif)

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_moderate"],
         s1$adj_bart_score[s1$cond_dist == "uniform"])

# Uniform vs. Normal Low SD
compare_diffs(mod_H3_normal_low, mod_H3_unif)

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_low"],
         s1$adj_bart_score[s1$cond_dist == "uniform"])

## Comparisons within the three "Normal" levels --

# Normal High SD vs. Normal Moderate SD
compare_diffs(mod_H3_normal_moderate, mod_H3_normal_high)

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_moderate"],
         s1$adj_bart_score[s1$cond_dist == "normal_high"])

# Normal Moderate SD vs. Normal Low SD
compare_diffs(mod_H3_normal_low, mod_H3_normal_moderate)

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_low"],
         s1$adj_bart_score[s1$cond_dist == "normal_moderate"])

## Uniform vs the three "Normal" levels: differences from optimal--
# Uniform vs. Normal High SD
diff_unif_norm_h_H3_opt <- tibble(
  delta_m = (mod_H3_normal_high$mu - 28) - (mod_H3_unif$mu - 32)
)

describe_posterior(diff_unif_norm_h_H3_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_h_H3_opt$delta_m))

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_high"] - 28,
         s1$adj_bart_score[s1$cond_dist == "uniform"] - 32)

# Uniform vs. Normal Moderate SD
diff_unif_norm_mod_H3_opt <- tibble(
  delta_m = (mod_H3_normal_moderate$mu - 25) - (mod_H3_unif$mu - 32)
)

describe_posterior(diff_unif_norm_mod_H3_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_mod_H3_opt$delta_m))

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_moderate"] - 25,
         s1$adj_bart_score[s1$cond_dist == "uniform"] - 32)

# Uniform vs. Normal Low SD
diff_unif_norm_l_H3_opt <- tibble(
  delta_m = mod_H3_normal_low$mu - mod_H3_unif$mu
)

describe_posterior(diff_unif_norm_l_H3_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_unif_norm_l_H3_opt$delta_m))

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_low"] - 25,
         s1$adj_bart_score[s1$cond_dist == "uniform"] - 32)

## Comparisons within the three "Normal" levels: relative to optimal --

# Normal High SD vs. Normal Moderate SD
diff_norm_h_norm_m_H3_opt <- tibble(
  delta_m = (mod_H3_normal_moderate$mu - 25) - (mod_H3_normal_high$mu - 28)
)

describe_posterior(diff_norm_h_norm_m_H3_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_norm_h_norm_m_H3_opt$delta_m))

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_moderate"] - 25,
         s1$adj_bart_score[s1$cond_dist == "normal_high"] - 28)

# Normal Moderate SD vs. Normal Low SD
diff_norm_m_norm_h_H3_opt <- tibble(
  delta_m = (mod_H3_normal_low$mu - 25) - (mod_H3_normal_moderate$mu - 25)
)

describe_posterior(diff_norm_m_norm_h_H3_opt$delta_m, test = c("rope"), ci = .95,
                   rope_ci = 1, rope_range = c(-.1, .1) * sd(diff_norm_m_norm_h_H3_opt$delta_m))

cohens_d(s1$adj_bart_score[s1$cond_dist == "normal_low"] - 25,
         s1$adj_bart_score[s1$cond_dist == "normal_moderate"] - 25)


# Plot the different distributions
s1 %>%
  mutate(cond_dist = factor(cond_dist, levels = c("uniform", "normal_high",
                                                  "normal_moderate", "normal_low"),
                            labels = c("Uniform", "Normal High SD", 
                                       "Normal Moderate SD", "Normal Low SD"))) %>%
  ggplot(aes(cond_dist, adj_bart_score)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .5) +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 2), 
               geom = "pointrange", color = "red", size = 1) +
  ylim(0, 65) +
  theme_bw() +
  labs(
    x = "Condition Levels",
    y = "Adjusted BART Scores"
  )


# Plot the means and standard deviations of the adjusted BART scores and the posterior beliefs ===========
plot_dat <- tibble(
  condition = c("unif", "norm_high", "norm_mod", "norm_low"),
  posterior_belief = c(describe_posterior(mod_H2_unif, ci = .95)$Median[1],
                       describe_posterior(mod_H2_normal_high, ci = .95)$Median[1],
                       describe_posterior(mod_H2_normal_moderate, ci = .95)$Median[1],
                       describe_posterior(mod_H2_normal_low, ci = .95)$Median[1]),
  posterior_belief_sd = c(describe_posterior(mod_H2_unif, ci = .95)$Median[3],
                          describe_posterior(mod_H2_normal_high, ci = .95)$Median[3],
                          describe_posterior(mod_H2_normal_moderate, ci = .95)$Median[3],
                          describe_posterior(mod_H2_normal_low, ci = .95)$Median[3]),
  adj_bart_score = c(describe_posterior(mod_H3_unif, ci = .95)$Median[1],
                       describe_posterior(mod_H3_normal_high, ci = .95)$Median[1],
                       describe_posterior(mod_H3_normal_moderate, ci = .95)$Median[1],
                       describe_posterior(mod_H3_normal_low, ci = .95)$Median[1]),
  adj_bart_score_sd = c(describe_posterior(mod_H3_unif, ci = .95)$Median[3],
                          describe_posterior(mod_H3_normal_high, ci = .95)$Median[3],
                          describe_posterior(mod_H3_normal_moderate, ci = .95)$Median[3],
                          describe_posterior(mod_H3_normal_low, ci = .95)$Median[3]),
)

# EV curves in the task design
balloons <- 1:64

# cumulative probabilities
cum_uniform_p <- punif(balloons, 1, 64)
cum_normal_p_high <- pnorm(balloons, 32, 18)
cum_normal_p_moderate <- pnorm(balloons, 32, 12)
cum_normal_p_low <- pnorm(balloons, 32, 6)

# EVs
ev_uniform_p <- balloons * (1 - cum_uniform_p)
ev_normal_p_high <- balloons * (1 - cum_normal_p_high)
ev_normal_p_moderate <- balloons * (1 - cum_normal_p_moderate)
ev_normal_p_low <- balloons * (1 - cum_normal_p_low)


ev_dat <- data.frame(unif = ev_uniform_p, norm_high = ev_normal_p_high,
                     norm_mod = ev_normal_p_moderate, norm_low = ev_normal_p_low)

cond <- c("unif", "norm_high", "norm_mod", "norm_low")

cols <- viridis(4, end = .8,  alpha = .8)
names(cols) <- cond

cols_poly <- viridis(4, end = .8,  alpha = .2)
names(cols_poly) <- cond

cols_lines <- viridis(4, end = .8,  alpha = 1)
names(cols_lines) <- cond

pchs <- 15:18
names(pchs) <- cond

texts <- paste0(letters[1:4], ")")
names(texts) <- cond

pdf("plots/s1/H2_H3_V2.pdf", height = 7, width = 9)

layout(matrix(c(1, 1, 2, 2, 5,
                3, 3, 4, 4, 5), byrow = TRUE, ncol = 5))
par(mar = c(4, 5, 3, 1))
titles <- c(expression(BART[uniform]), expression(BART[normal-H]),
            expression(BART[normal-M]), expression(BART[normal-L]))
names(titles) = c("unif", "norm_high", "norm_mod", "norm_low")
# Plot Uniform condition
for (cond_i in cond) {
  
  plot(balloons, ev_dat[[cond_i]], ylim = c(0, 22), bty = "l", las = 1, cex.axis = 1.4,
       cex.lab = 1.8, xlab = "", ylab = "", xlim = c(0.5, 64.5), xaxs = "i",
       pch = pchs[cond_i], col = cols[cond_i])
  mtext("Expected Payoff", 2, cex = 1.2, line = 3, padj = -.5)
  mtext("N Inflations", 1, cex = 1.2, line = 3, padj = -.25)
  mtext(titles[cond_i], 3, cex = 1.25, adj = 0, padj = -.4)
  # text(titles[cond_i], font = 2, xpd = TRUE, x = -13, y = 24, cex = 2, offset=0,
  #      adj = 0)
#  text(texts[cond_i], font = 2, xpd = TRUE, x = -17, y = 22, cex = 2)
  
  polygon(x = c(-1, -1, 1, 1) * 
            plot_dat$posterior_belief_sd[plot_dat$condition == cond_i] + 
            plot_dat$posterior_belief[plot_dat$condition == cond_i],
          y = c(-2, 25, 25, -2), col = cols_poly[cond_i], border = FALSE)
  polygon(x = c(-1, -1, 1, 1) * plot_dat$adj_bart_score_sd[plot_dat$condition == cond_i] + 
            plot_dat$adj_bart_score[plot_dat$condition == cond_i],
          y = c(-2, 25, 25, -2), col = cols_poly[cond_i], border = FALSE)
  
  abline(v = plot_dat$posterior_belief[plot_dat$condition == cond_i],
         col = cols_lines[cond_i], lwd = 3)
  abline(v = plot_dat$adj_bart_score[plot_dat$condition == cond_i],
         col = cols_lines[cond_i], lwd = 3, lty = 2)
  
}
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
segments(c(.01, .01), c(.4, .6), c(.15, .15), c(.4, .6), lty = c(2, 1),
         xpd = TRUE, lwd = 2)

text(c("actual behavior\n(mean adjusted\n BART scores)", "belief about\noptimal behavior\n(mean rating)"),
     x = c(.23, .23), y = c(.4, .6), adj = 0, offset = 0, cex = 1.5)

dev.off()

### H4 =========================================================================

# H4: Convergent validity: As the distribution of adjusted BART scores in the
#     BARTnormal potentially reflects individual difference in risk preference more
#     directly, we expect a higher convergent validity between adjusted BART scores
#     and the other measures of risk preference (propensity and frequency measures)
#     in the BARTnormal as compared to in the BARTuniform.


### correlations with adjusted BART scores =====================================

dvs <- names(s1)[c(19:26, 33:38)]

cors <- list()

for (i in seq_along(dvs)) {
  
  cor_unif <- correlationBF(s1$adj_bart_score[s1$cond_dist == "uniform"],
                            s1[[dvs[i]]][s1$cond_dist == "uniform"],
                            iterations = 10000)
  posterior_unif <- describe_posterior(cor_unif, rope_range = c(-.05, .05),
                                       ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_h <- correlationBF(s1$adj_bart_score[s1$cond_dist == "normal_high"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_high"],
                              iterations = 10000)
  posterior_norm_h <- describe_posterior(cor_norm_h, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_m <- correlationBF(s1$adj_bart_score[s1$cond_dist == "normal_moderate"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_moderate"],
                              iterations = 10000)
  posterior_norm_m <- describe_posterior(cor_norm_m, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_l <- correlationBF(s1$adj_bart_score[s1$cond_dist == "normal_low"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_low"],
                              iterations = 10000)
  posterior_norm_l <- describe_posterior(cor_norm_l, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  # compute differences between correlations
  delta_unif_norm_h <- compare_cors(cor_norm_h, cor_unif, raw = TRUE)
  
  delta_unif_norm_m <- compare_cors(cor_norm_m, cor_unif, raw = TRUE)
  
  delta_unif_norm_l <- compare_cors(cor_norm_l, cor_unif, raw = TRUE)
  
  delta_norm_h_norm_m <- compare_cors(cor_norm_m, cor_norm_h, raw = TRUE)
  
  delta_norm_m_norm_l <- compare_cors(cor_norm_l, cor_norm_m, raw = TRUE)
  

  cors[[i]] <- data.frame(
    dv = dvs[i],
    unif = posterior_unif$Median,
    unif_low = posterior_unif$CI_low,
    unif_high = posterior_unif$CI_high,
    unif_rope_p = posterior_unif$ROPE_Percentage,
    
    norm_h = posterior_norm_h$Median,
    norm_h_low = posterior_norm_h$CI_low,
    norm_h_high = posterior_norm_h$CI_high,
    norm_h_rope_p = posterior_norm_h$ROPE_Percentage,
  
    norm_m = posterior_norm_m$Median,
    norm_m_low = posterior_norm_m$CI_low,
    norm_m_high = posterior_norm_m$CI_high,
    norm_m_rope_p = posterior_norm_m$ROPE_Percentage,
  
    norm_l = posterior_norm_l$Median,
    norm_l_low = posterior_norm_l$CI_low,
    norm_l_high = posterior_norm_l$CI_high,
    norm_l_rope_p = posterior_norm_l$ROPE_Percentage,
    
    delta_unif_norm_h = delta_unif_norm_h$Median,
    delta_unif_norm_h_low = delta_unif_norm_h$CI_low,
    delta_unif_norm_h_high = delta_unif_norm_h$CI_high,
    delta_unif_norm_h_rope_p = delta_unif_norm_h$ROPE_Percentage,
    
    delta_unif_norm_m = delta_unif_norm_m$Median,
    delta_unif_norm_m_low = delta_unif_norm_m$CI_low,
    delta_unif_norm_m_high = delta_unif_norm_m$CI_high,
    delta_unif_norm_m_rope_p = delta_unif_norm_m$ROPE_Percentage,
    
    delta_unif_norm_l = delta_unif_norm_l$Median,
    delta_unif_norm_l_low = delta_unif_norm_l$CI_low,
    delta_unif_norm_l_high = delta_unif_norm_l$CI_high,
    delta_unif_norm_l_rope_p = delta_unif_norm_l$ROPE_Percentage,
    
    delta_norm_h_norm_m = delta_norm_h_norm_m$Median,
    delta_norm_h_norm_m_low = delta_norm_h_norm_m$CI_low,
    delta_norm_h_norm_m_high = delta_norm_h_norm_m$CI_high,
    delta_norm_h_norm_m_rope_p = delta_norm_h_norm_m$ROPE_Percentage,
    
    delta_norm_m_norm_l = delta_norm_m_norm_l$Median,
    delta_norm_m_norm_l_low = delta_norm_m_norm_l$CI_low,
    delta_norm_m_norm_l_high = delta_norm_m_norm_l$CI_high,
    delta_norm_m_norm_l_rope_p = delta_norm_m_norm_l$ROPE_Percentage
  )

}

cors_adj <- do.call(rbind, cors)

# Plot correlations


conclusive <- cors_adj %>%
  mutate(evidence_unif = case_when(delta_unif_norm_h_rope_p < .025 ~ "Different",
                                   delta_unif_norm_h_rope_p > .975 ~ "Equal",
                                   TRUE ~ "Inconclusive"),
         evidence_norm_h = case_when(delta_unif_norm_h_rope_p < .025 ~ "Different",
                                     delta_unif_norm_h_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive"),
         evidence_norm_m = case_when(delta_unif_norm_m_rope_p < .025 ~ "Different",
                                     delta_unif_norm_m_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive"),
         evidence_norm_l = case_when(delta_unif_norm_l_rope_p < .025 ~ "Different",
                                     delta_unif_norm_l_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive")) %>%
  select(dv, evidence_unif:evidence_norm_l) %>%
  pivot_longer(evidence_unif:evidence_norm_l,
               names_to = "distribution", names_prefix = "evidence_",
               values_to = "Evidence")


temp_adj <- cors_adj %>%
  select(dv, unif, norm_h, norm_m, norm_l) %>%
  pivot_longer(unif:norm_l, names_to = "distribution", values_to = "Correlation") %>%
  left_join(conclusive, by = c("dv", "distribution")) %>%
  filter(distribution %in% c("unif", "norm_m")) %>%
  mutate(distribution = factor(distribution,
                               levels = c("unif", "norm_m"),
                               labels = c("BARTuniform", "BARTnormal-M")),
         Evidence = factor(Evidence),
         mtype = case_when(grepl("grips|soep", dv) ~ "prop",
                           TRUE ~ "freq"),
         dv = case_when(dv == "grips_score" ~ "GRiPS",
                        dv == "soep_gen" ~ "SOEP[general]",
                        dv == "soep_driving" ~ "SOEP[driving]",
                        dv == "soep_finance" ~ "SOEP[finance]",
                        dv == "soep_health" ~ "SOEP[health]",
                        dv == "soep_leisure" ~ "SOEP[leisure]",
                        dv == "soep_occupation" ~ "SOEP[occupation]",
                        dv == "soep_faith" ~ "SOEP[social]",
                        dv == "rlrisk_drink_b" ~ "Drinking",
                        dv == "rlrisk_gamble_b" ~ "Gambling",
                        dv == "rlrisk_invest_b" ~ "Investing",
                        dv == "rlrisk_cigarettes_b" ~ "Smoking",
                        dv == "rlrisk_speed_b" ~ "Speeding",
                        dv == "rlrisk_sport_b" ~ "Sport"))
p1 <- ggplot(temp_adj, aes(distribution, Correlation, group = dv, label = dv)) +
  geom_hline(yintercept = 0, lty = 2, size = 1.25, col = "#646464", alpha = .6) +
  geom_hline(yintercept = -.05, lty = 3, size = 1.25, col = "#646464", alpha = .6) +
  geom_hline(yintercept = .05, lty = 3, size = 1.25, col = "#646464", alpha = .6) +
  geom_point(shape = 16, size = 2) +
  stat_summary(aes(group = distribution), fun.y = "mean", geom = "point", size = 3,
               col = "black") +
  geom_segment(aes(x = 1, y = mean(cors_adj$unif), xend = 2, yend = mean(cors_adj$norm_m)),
               size = 1, colour = "black") +
  geom_line(col = "#646464", alpha = .3) +
  # geom_label_repel(data= temp_adj %>% filter(distribution == "Uniform"),
  #                  aes(fill = mtype), parse = TRUE, show.legend = FALSE,
  #                  point.padding = .25, nudge_x = -.35) +
  geom_label_repel(data= temp_adj %>% filter(distribution == "BARTnormal-M"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 6, xlim = c(2.05, 2.45), ylim = c(-.18, .38)) +
  scale_fill_manual(values = c("#62c86c", "#fbe94b")) +
  theme_classic() +
  coord_cartesian(ylim=c(-.1,.35)) +
  labs(
    x = "",
    y = "Correlation with adjusted BART score"
  ) +
  theme(
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  )

# compute average correlations
apply(cors_adj[, c("unif", "norm_h", "norm_m", "norm_l")], 2, mean)

# compute average differences in correlations
apply(cors_adj[, c("delta_unif_norm_h", "delta_unif_norm_m", "delta_unif_norm_l")],
      2, mean)


### Correlations number of explosions ==========================================

dvs <- names(s1)[c(19:26,33:38)]

cors <- list()

for (i in seq_along(dvs)) {
  
  cor_unif <- correlationBF(s1$n_explosions[s1$cond_dist == "uniform"],
                            s1[[dvs[i]]][s1$cond_dist == "uniform"],
                            iterations = 10000)
  posterior_unif <- describe_posterior(cor_unif, rope_range = c(-.05, .05),
                                       ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_h <- correlationBF(s1$n_explosions[s1$cond_dist == "normal_high"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_high"],
                              iterations = 10000)
  posterior_norm_h <- describe_posterior(cor_norm_h, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_m <- correlationBF(s1$n_explosions[s1$cond_dist == "normal_moderate"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_moderate"],
                              iterations = 10000)
  posterior_norm_m <- describe_posterior(cor_norm_m, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_l <- correlationBF(s1$n_explosions[s1$cond_dist == "normal_low"],
                              s1[[dvs[i]]][s1$cond_dist == "normal_low"],
                              iterations = 10000)
  posterior_norm_l <- describe_posterior(cor_norm_l, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  # compute differences between correlations
  delta_unif_norm_h <- compare_cors(cor_norm_h, cor_unif, raw = TRUE)
  
  delta_unif_norm_m <- compare_cors(cor_norm_m, cor_unif, raw = TRUE)
  
  delta_unif_norm_l <- compare_cors(cor_norm_l, cor_unif, raw = TRUE)
  
  delta_norm_h_norm_m <- compare_cors(cor_norm_m, cor_norm_h, raw = TRUE)
  
  delta_norm_m_norm_l <- compare_cors(cor_norm_l, cor_norm_m, raw = TRUE)
  
  cors[[i]] <- data.frame(
    dv = dvs[i],
    unif = posterior_unif$Median,
    unif_low = posterior_unif$CI_low,
    unif_high = posterior_unif$CI_high,
    unif_rope_p = posterior_unif$ROPE_Percentage,
    
    norm_h = posterior_norm_h$Median,
    norm_h_low = posterior_norm_h$CI_low,
    norm_h_high = posterior_norm_h$CI_high,
    norm_h_rope_p = posterior_norm_h$ROPE_Percentage,
    
    norm_m = posterior_norm_m$Median,
    norm_m_low = posterior_norm_m$CI_low,
    norm_m_high = posterior_norm_m$CI_high,
    norm_m_rope_p = posterior_norm_m$ROPE_Percentage,
    
    norm_l = posterior_norm_l$Median,
    norm_l_low = posterior_norm_l$CI_low,
    norm_l_high = posterior_norm_l$CI_high,
    norm_l_rope_p = posterior_norm_l$ROPE_Percentage,
    
    delta_unif_norm_h = delta_unif_norm_h$Median,
    delta_unif_norm_h_low = delta_unif_norm_h$CI_low,
    delta_unif_norm_h_high = delta_unif_norm_h$CI_high,
    delta_unif_norm_h_rope_p = delta_unif_norm_h$ROPE_Percentage,
    
    delta_unif_norm_m = delta_unif_norm_m$Median,
    delta_unif_norm_m_low = delta_unif_norm_m$CI_low,
    delta_unif_norm_m_high = delta_unif_norm_m$CI_high,
    delta_unif_norm_m_rope_p = delta_unif_norm_m$ROPE_Percentage,
    
    delta_unif_norm_l = delta_unif_norm_l$Median,
    delta_unif_norm_l_low = delta_unif_norm_l$CI_low,
    delta_unif_norm_l_high = delta_unif_norm_l$CI_high,
    delta_unif_norm_l_rope_p = delta_unif_norm_l$ROPE_Percentage,
    
    delta_norm_h_norm_m = delta_norm_h_norm_m$Median,
    delta_norm_h_norm_m_low = delta_norm_h_norm_m$CI_low,
    delta_norm_h_norm_m_high = delta_norm_h_norm_m$CI_high,
    delta_norm_h_norm_m_rope_p = delta_norm_h_norm_m$ROPE_Percentage,
    
    delta_norm_m_norm_l = delta_norm_m_norm_l$Median,
    delta_norm_m_norm_l_low = delta_norm_m_norm_l$CI_low,
    delta_norm_m_norm_l_high = delta_norm_m_norm_l$CI_high,
    delta_norm_m_norm_l_rope_p = delta_norm_m_norm_l$ROPE_Percentage
  )
  
}

cors_n_exp <- do.call(rbind, cors)

# Plot correlations

conclusive <- cors_n_exp %>%
  mutate(evidence_unif = case_when(unif_rope_p < .025 ~ "Different",
                                   unif_rope_p > .975 ~ "Equal",
                                   TRUE ~ "Inconclusive"),
         evidence_norm_h = case_when(delta_unif_norm_h_rope_p < .025 ~ "Different",
                                     delta_unif_norm_h_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive"),
         evidence_norm_m = case_when(delta_unif_norm_m_rope_p < .025 ~ "Different",
                                     delta_unif_norm_m_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive"),
         evidence_norm_l = case_when(delta_unif_norm_l_rope_p < .025 ~ "Different",
                                     delta_unif_norm_l_rope_p > .975 ~ "Equal",
                                     TRUE ~ "Inconclusive")) %>%
  select(dv, evidence_unif:evidence_norm_l) %>%
  pivot_longer(evidence_unif:evidence_norm_l,
               names_to = "distribution", names_prefix = "evidence_",
               values_to = "Evidence")



temp_n_exp <- cors_n_exp %>%
  select(dv, unif, norm_h, norm_m, norm_l) %>%
  pivot_longer(unif:norm_l, names_to = "distribution", values_to = "Correlation") %>%
  left_join(conclusive, by = c("dv", "distribution")) %>%
  filter(distribution %in% c("unif", "norm_m")) %>%
  mutate(distribution = factor(distribution,
                               levels = c("unif", "norm_m"),
                               labels = c("BARTuniform", "BARTnormal-M")),
         Evidence = factor(Evidence),
         mtype = case_when(grepl("grips|soep", dv) ~ "prop",
                           TRUE ~ "freq"),
         dv = case_when(dv == "grips_score" ~ "GRiPS",
                        dv == "soep_gen" ~ "SOEP[general]",
                        dv == "soep_driving" ~ "SOEP[driving]",
                        dv == "soep_finance" ~ "SOEP[finance]",
                        dv == "soep_health" ~ "SOEP[health]",
                        dv == "soep_leisure" ~ "SOEP[leisure]",
                        dv == "soep_occupation" ~ "SOEP[occupation]",
                        dv == "soep_faith" ~ "SOEP[social]",
                        dv == "rlrisk_drink_b" ~ "Drinking",
                        dv == "rlrisk_gamble_b" ~ "Gambling",
                        dv == "rlrisk_invest_b" ~ "Investing",
                        dv == "rlrisk_cigarettes_b" ~ "Smoking",
                        dv == "rlrisk_speed_b" ~ "Speeding",
                        dv == "rlrisk_sport_b" ~ "Sport"))
p2 <- ggplot(temp_n_exp, aes(distribution, Correlation, group = dv, label = dv)) +
  geom_hline(yintercept = 0, lty = 2, size = 1.25, col = "#646464", alpha = .6) +
  geom_hline(yintercept = -.05, lty = 3, size = 1.25, col = "#646464", alpha = .6) +
  geom_hline(yintercept = .05, lty = 3, size = 1.25, col = "#646464", alpha = .6) +
  geom_point(shape = 16, size = 2) +
  stat_summary(aes(group = distribution), fun.y = "mean", geom = "point", size = 3,
               col = "black") +
  geom_segment(aes(x = 1, y = mean(cors_n_exp$unif), xend = 2, yend = mean(cors_n_exp$norm_m)),
               size = 1, colour = "black") +
  geom_line(col = "#646464", alpha = .3) +
  # geom_label_repel(data= temp_n_exp %>% filter(distribution == "Uniform"),
  #                  aes(fill = mtype), parse = TRUE, show.legend = FALSE,
  #                  point.padding = .25, nudge_x = -.35) +
  geom_label_repel(data= temp_n_exp %>% filter(distribution == "BARTnormal-M"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 6, xlim = c(2.05, 2.4)) +
  scale_fill_manual(values = c("#62c86c", "#fbe94b")) +
  theme_classic() +
  coord_cartesian(ylim=c(-.1,.35)) +
  labs(
    x = "",
    y = "Correlation with N Explosions"
  ) +
  theme(
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  )

pdf("plots/s1/H4.pdf", height = 8, width = 16)
p1 + p2
dev.off()

# compute average correlations
apply(cors_n_exp[, c("unif", "norm_h", "norm_m", "norm_l")], 2, mean)

# compute average differences in correlations
apply(cors_n_exp[, c("delta_unif_norm_h", "delta_unif_norm_m", "delta_unif_norm_l")],
      2, mean)

### Plot BART distributions ====================================================

dists_l <- dists %>%
  pivot_longer(everything(), names_to = "cond_dist", values_to = "values") %>%
  mutate(cond_dist = factor(cond_dist, levels = c("normal_low", "normal_moderate",
                                                  "normal_high", "uniform")))

pdf("plots/s1/bart_distributions.pdf", height = 4, width = 10)

par(mfrow = c(1, 2))

cols <- viridis(4, end = .8,  alpha = .8)
names(cols) <- c("uniform", "normal_high",
                 "normal_moderate", "normal_low")
cols_m <- viridis(4, end = .8,  alpha = 1)
names(cols_m) <- c("uniform", "normal_high",
                   "normal_moderate", "normal_low")
ord <- c("normal_low", "normal_moderate",
         "normal_high", "uniform")

par(mar = c(3.5, 6.75, 0, .1))
plot.new()
plot.window(xlim = c(1, 64), ylim = c(1, 4.7))
abline(v = 32, col = "darkgrey", lty = 3, lwd = 2.5)
axis(1, at = seq(0, 64, 8), cex.axis = 1, lwd = 2)
text(c(expression(BART[normal-L]), expression(BART[normal-M]),
       expression(BART[normal-H]), expression(BART[uniform])),
     y = c(1.08, 2.08, 3.08, 4.08), x = -26, xpd = TRUE, offset = 0, adj = 0,
     cex = 1.25)
mtext("Maximum Capacity", side = 1, line = 3, cex = 1.25, padj = -.65)
stripchart(values ~ cond_dist, data = dists_l, method = "stack",
           pch=16, col = cols[ord], add = TRUE) 

par(mar = c(3.5, 4, 0.5, .1))
plot.new()
plot.window(xlim = c(.5, 30.5), ylim = c(.5, 64.5), xaxs = "i", yaxs = "i")
axis(2, at = c(0,1, seq(8, 64, 8)), cex.axis = 1, lwd = 2, las = 1)
axis(1, at = c(0, 1, seq(5, 30, 5)), cex.axis = 1, lwd = 2)
mtext("Trial", side = 1, line = 3, cex = 1.25, padj = -.65)
mtext("Maximum Capacity", side = 2, line = 2, cex = 1.25, padj = -.5)
lines(1:30, dists$uniform, col = cols["uniform"], lwd = 3, type = "b", pch = 16)
lines(1:30, dists$normal_low, col = cols["normal_low"], lwd = 3, type = "b", pch = 16)
lines(1:30, dists$normal_moderate, col = cols["normal_moderate"], lwd = 3, type = "b", pch = 16)
lines(1:30, dists$normal_high, col = cols["normal_high"], lwd = 3, type = "b", pch = 16)

points(1:30, dists$uniform, col = cols["uniform"], pch = 16)
points(1:30, dists$normal_low, col = cols["normal_low"], pch = 16)
points(1:30, dists$normal_moderate, col = cols["normal_moderate"], pch = 16)
points(1:30, dists$normal_high, col = cols["normal_high"], pch = 16)

dev.off()
