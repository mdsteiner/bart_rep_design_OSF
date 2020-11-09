### Analysis script study 2

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
s2 <- readRDS("data/s2/study_2.RDS")
bart <- read_csv("data/s2/bart_results.csv")
bart <- bart %>%
  filter(partid %in% unique(s2$partid)) %>%
  distinct(partid, trial, .keep_all = TRUE) %>%
  left_join(s2 %>% select(partid, cond_dist, distribution_rating), by = "partid")

### Demographics ===============================================================

prop.table(table(s2$sex))

mean(s2$age)
sd(s2$age)

prop.table(table(s2$education))

prop.table(table(s2$job))
table(s2$cond_dist)

### Descriptive Statistics =====================================================

# completion time
mean(s2$duration)
#payoff
mean(s2$bart_payoff) + .1

s2 %>%
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
    m_ss = mean(ss),
    m_pers = mean(perseverance),
    m_prem = mean(premeditation),
    m_urg = mean(urgency),
    N = n()
  ) %>%
  View()

# for table in SM
s2 %>%
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
    upps_ss = paste0(round(mean(ss), 2), " (",
                             round(sd(ss), 2), ")"),
    upps_perseverance = paste0(round(mean(perseverance), 2), " (",
                             round(sd(perseverance), 2), ")"),
    upps_premeditation = paste0(round(mean(premeditation), 2), " (",
                             round(sd(premeditation), 2), ")"),
    upps_urgency = paste0(round(mean(urgency), 2), " (",
                             round(sd(urgency), 2), ")"),
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
s2 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  pivot_longer(ends_with("_b"), names_to = "rlrisk", values_to = "freq") %>%
  ggplot(aes(freq)) +
  geom_histogram() +
  facet_grid(cond_dist ~ rlrisk)

s2 %>%
  mutate(cond_dist = factor(cond_dist,
                            levels = c("uniform", "normal_high",
                                       "normal_moderate", "normal_low"))) %>%
  pivot_longer(starts_with("soep"), names_to = "soep", values_to = "rating") %>%
  ggplot(aes(rating)) +
  geom_histogram() +
  facet_grid(cond_dist ~ soep)


# correlations between different risk preference measures
s2 %>%
  select(adj_bart_score, posterior_belief, grips_score, starts_with("soep"),
         ends_with("_b"), premeditation, perseverance, urgency, ss) %>%
  pairs.panels(method = "spearman")

pdf("plots/s2/cors_measures.pdf", height = 6, width = 14)
ps1 <- s1 %>%
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

ps2 <- s2 %>%
  select(grips_score, starts_with("soep"), -ends_with("_s1"),
         ends_with("_b"), premeditation, perseverance, urgency, ss) %>%
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
         Sport = rlrisk_sport_b,
         Premeditation = premeditation,
         Perseverance = perseverance,
         Urgency = urgency,
         `SensationSeeking` = ss) %>%
  ggcorr(label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE,
         hjust = 0.9, method = c("pairwise", "spearman"),
         name = expression(italic(r[s])), palette = "PuOr", layout.exp = 1)

ps1 + ps2
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

pdf("plots/s2/bart_behavior.pdf", height = 6, width = 10)
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
  # stat_cor(aes(group = 1, label = paste0("italic(r[s])==", ..r..)),
  #          method = "spearman", label.x = 3, label.y = 58, geom = "label",
  #          size = 4) +
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


# Plot posterior beliefs

s2 %>%
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

# Plot the distributions of adjusted BART scores
s2 %>%
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

### H1 =========================================================================

# H1: General task representation: At the end of the task, participants will
#     believe that the explosion points cluster around a mean value rather than that
#     they are randomly distributed, irrespective of the actual distributional form
#     implemented (i.e., BARTuniform vs. BARTnormal). Moreover, within BARTnormal,
#     we expect this belief to be increasingly stronger the smaller the standard
#     deviation of the distributions becomes.

H1_dat <- s2 %>%
  select(distribution_rating, cond_dist) %>%
  mutate(cond_dist = factor(cond_dist, levels = c("uniform", "normal_high",
                                                  "normal_moderate", "normal_low")))
H1_dat %>%
  group_by(cond_dist) %>%
  summarise(
    p_normal = sum(distribution_rating > 0) / (sum(distribution_rating > 0) +
                                                  sum(distribution_rating < 0)) * 100
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

pdf("plots/s2/H1_V2.pdf", height = 6, width = 10)
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
plot.window(xlim = c(-50, 50), ylim = c(1, 4.7))
abline(v = 0, col = "darkgrey", lty = 3, lwd = 2.5)
axis(1, at = seq(-50, 50, 10), cex.axis = 1, lwd = 2)
text(c(expression(BART[normal-L]), expression(BART[normal-M]),
       expression(BART[normal-H]), expression(BART[uniform])),
     y = c(1.25, 2.25, 3.25, 4.25), x = -69.5, xpd = TRUE, offset = 0, adj = 0,
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

text(c("Uniform", "Normal"), y = rep(.43, 2), x = c(-50, 50),  xpd = TRUE,
     offset = 0, adj = .5, cex = 1.25)

dev.off()

### H4 =========================================================================

# H4: Convergent validity: As the distribution of adjusted BART scores in the
#     BARTnormal potentially reflects individual difference in risk preference more
#     directly, we expect a higher convergent validity between adjusted BART scores
#     and the other measures of risk preference (propensity and frequency measures)
#     in the BARTnormal as compared to in the BARTuniform.


### correlations with adjusted BART scores =====================================

dvs <- names(s2)[c(13:20, 27:36)]

cors <- list()

for (i in seq_along(dvs)) {
  
  cor_unif <- correlationBF(s2$adj_bart_score[s2$cond_dist == "uniform"],
                            s2[[dvs[i]]][s2$cond_dist == "uniform"],
                            iterations = 10000)
  posterior_unif <- describe_posterior(cor_unif, rope_range = c(-.05, .05),
                                       ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_h <- correlationBF(s2$adj_bart_score[s2$cond_dist == "normal_high"],
                            s2[[dvs[i]]][s2$cond_dist == "normal_high"],
                            iterations = 10000)
  posterior_norm_h <- describe_posterior(cor_norm_h, rope_range = c(-.05, .05),
                                       ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_m <- correlationBF(s2$adj_bart_score[s2$cond_dist == "normal_moderate"],
                                   s2[[dvs[i]]][s2$cond_dist == "normal_moderate"],
                                   iterations = 10000)
  posterior_norm_m <- describe_posterior(cor_norm_m, rope_range = c(-.05, .05),
                                              ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_l <- correlationBF(s2$adj_bart_score[s2$cond_dist == "normal_low"],
                                   s2[[dvs[i]]][s2$cond_dist == "normal_low"],
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
                               labels = c("Uniform", "Normal Moderate SD")),
         Evidence = factor(Evidence),
         mtype = case_when(grepl("rlrisk", dv) ~ "freq",
                           TRUE ~ "prop"),
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
                        dv == "rlrisk_sport_b" ~ "Sport",
                        dv == "premeditation" ~ "Premeditation",
                        dv == "perseverance" ~"Perseverance",
                        dv == "urgency" ~ "Urgency",
                        dv == "ss" ~ "SenationSeeking"))

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
  geom_label_repel(data= temp_adj %>% filter(distribution == "Normal Moderate SD" & mtype == "prop"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 5, xlim = c(2.1, 2.55), ylim = c(-3, .38)) +
  geom_label_repel(data= temp_adj %>% filter(distribution == "Uniform" & mtype == "freq"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 5, xlim = c(-.0, .95), ylim = c(-3, .38)) +
  scale_fill_manual(values = c("#62c86c", "#fbe94b")) +
  theme_classic() +
  coord_cartesian(ylim=c(-.35,.35), xlim = c(1.1, 2.4)) +
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

dvs <- names(s2)[c(13:20, 27:36)]

cors <- list()

for (i in seq_along(dvs)) {
  
  cor_unif <- correlationBF(s2$n_explosions[s2$cond_dist == "uniform"],
                            s2[[dvs[i]]][s2$cond_dist == "uniform"],
                            iterations = 10000)
  posterior_unif <- describe_posterior(cor_unif, rope_range = c(-.05, .05),
                                       ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_h <- correlationBF(s2$n_explosions[s2$cond_dist == "normal_high"],
                              s2[[dvs[i]]][s2$cond_dist == "normal_high"],
                              iterations = 10000)
  posterior_norm_h <- describe_posterior(cor_norm_h, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_m <- correlationBF(s2$n_explosions[s2$cond_dist == "normal_moderate"],
                              s2[[dvs[i]]][s2$cond_dist == "normal_moderate"],
                              iterations = 10000)
  posterior_norm_m <- describe_posterior(cor_norm_m, rope_range = c(-.05, .05),
                                         ci = .95, rope_ci = 1, centrality = "all")
  
  cor_norm_l <- correlationBF(s2$n_explosions[s2$cond_dist == "normal_low"],
                              s2[[dvs[i]]][s2$cond_dist == "normal_low"],
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
                               labels = c("Uniform", "Normal Moderate SD")),
         Evidence = factor(Evidence),
         mtype = case_when(grepl("rlrisk", dv) ~ "freq",
                           TRUE ~ "prop"),
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
                        dv == "rlrisk_sport_b" ~ "Sport",
                        dv == "premeditation" ~ "Premeditation",
                        dv == "perseverance" ~"Perseverance",
                        dv == "urgency" ~ "Urgency",
                        dv == "ss" ~ "SenationSeeking"))
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
  geom_label_repel(data= temp_n_exp %>% filter(distribution == "Normal Moderate SD" & mtype == "prop"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 5, xlim = c(2.1, 2.55), ylim = c(-3, .38)) +
  geom_label_repel(data= temp_n_exp %>% filter(distribution == "Uniform" & mtype == "freq"),
                   aes(fill = mtype), parse = TRUE, show.legend = FALSE,
                   size = 5, xlim = c(-.0, .95), ylim = c(-3, .38)) +
  scale_fill_manual(values = c("#62c86c", "#fbe94b")) +
  theme_classic() +
  coord_cartesian(ylim=c(-.35,.35), xlim = c(1.1, 2.4)) +
  labs(
    x = "",
    y = "Correlation with N Explosions"
  ) +
  theme(
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  )

pdf("plots/s2/H4.pdf", height = 8, width = 16)
p1 + p2
dev.off()
  
# compute average correlations
apply(cors_n_exp[, c("unif", "norm_h", "norm_m", "norm_l")], 2, mean)

# compute average differences in correlations
apply(cors_n_exp[, c("delta_unif_norm_h", "delta_unif_norm_m", "delta_unif_norm_l")],
      2, mean)

### H5 =========================================================================

# H5: Reliability of beliefs about optimal behavior: Participants' beliefs about
#     the optimal value will exhibit a higher test-retest reliability in the
#     BARTnormal as opposed to in the BARTuniform. Moreover, the test-retest 
#     reliability within the BARTnormal will be higher, the lower the standard
#     deviation of the explosion points.

### First compute test-restest reliabilities --

# BARTuniform
unif_H5 <- get_rel(s2$posterior_belief[s2$cond_dist == "uniform"],
                   s2$posterior_belief_s1[s2$cond_dist == "uniform"])

# BARTnormal-H
norm_h_H5 <- get_rel(s2$posterior_belief[s2$cond_dist == "normal_high"],
                   s2$posterior_belief_s1[s2$cond_dist == "normal_high"])

# BARTnormal-M
norm_m_H5 <- get_rel(s2$posterior_belief[s2$cond_dist == "normal_moderate"],
                   s2$posterior_belief_s1[s2$cond_dist == "normal_moderate"])

# BARTnormal-L
norm_l_H5 <- get_rel(s2$posterior_belief[s2$cond_dist == "normal_low"],
                   s2$posterior_belief_s1[s2$cond_dist == "normal_low"])

### compute differences between test-retest reliabilities --

## Comparisons between BARTnormal and BARTuniform
# normal high
compare_cors(norm_h_H5$full, unif_H5$full)
 
# normal moderate
compare_cors(norm_m_H5$full, unif_H5$full)

# normal low
compare_cors(norm_l_H5$full, unif_H5$full)

## Comparisons within BARTnormal
# normal moderate vs normal high
compare_cors(norm_m_H5$full, norm_h_H5$full)

# normal low vs normal high
compare_cors(norm_l_H5$full, norm_h_H5$full)

# normal low vsnormal moderate
compare_cors(norm_l_H5$full, norm_m_H5$full)

### H6 =========================================================================
# H6: Reliability of observed risk-taking behavior: There will be a higher
#     test-retest reliability of the adjusted BART scores  and the total number
#     of explosions in the BARTnormal as compared to in the BARTuniform.
#     Moreover, the test-retest reliability within the BARTnormal will be higher,
#     the lower the standard deviation of the explosion points.

### adjusted BART scores ==
### First compute test-restest reliabilities --

# BARTuniform
unif_H6 <- get_rel(s2$adj_bart_score[s2$cond_dist == "uniform"],
                   s2$adj_bart_score_s1[s2$cond_dist == "uniform"])

# BARTnormal-H
norm_h_H6 <- get_rel(s2$adj_bart_score[s2$cond_dist == "normal_high"],
                       s2$adj_bart_score_s1[s2$cond_dist == "normal_high"])

# BARTnormal-M
norm_m_H6 <- get_rel(s2$adj_bart_score[s2$cond_dist == "normal_moderate"],
                       s2$adj_bart_score_s1[s2$cond_dist == "normal_moderate"])


# BARTnormal-L
norm_l_H6 <- get_rel(s2$adj_bart_score[s2$cond_dist == "normal_low"],
                       s2$adj_bart_score_s1[s2$cond_dist == "normal_low"])

### compute differences between test-retest reliabilities --

## Comparisons between BARTnormal and BARTuniform
# normal high
compare_cors(norm_h_H6$full, unif_H6$full)

# normal moderate
compare_cors(norm_m_H6$full, unif_H6$full)

# normal low
compare_cors(norm_l_H6$full, unif_H6$full)

## Comparisons within BARTnormal
# normal high vs normal moderate
compare_cors(norm_m_H6$full, norm_h_H6$full)

# normal low vs normal moderate
compare_cors(norm_l_H6$full, norm_m_H6$full)

# normal low vs normal high
compare_cors(norm_l_H6$full, norm_h_H6$full)

### n explosions ==
### First compute test-restest reliabilities --

# BARTuniform
unif_H6_expl <- get_rel(s2$n_explosions[s2$cond_dist == "uniform"],
                        s2$n_explosions_s1[s2$cond_dist == "uniform"])

# BARTnormal-H
norm_h_H6_expl <- get_rel(s2$n_explosions[s2$cond_dist == "normal_high"],
                     s2$n_explosions_s1[s2$cond_dist == "normal_high"])

# BARTnormal-M
norm_m_H6_expl <- get_rel(s2$n_explosions[s2$cond_dist == "normal_moderate"],
                     s2$n_explosions_s1[s2$cond_dist == "normal_moderate"])


# BARTnormal-L
norm_l_H6_expl <- get_rel(s2$n_explosions[s2$cond_dist == "normal_low"],
                     s2$n_explosions_s1[s2$cond_dist == "normal_low"])

### compute differences between test-retest reliabilities --

## Comparisons between BARTnormal and BARTuniform
# normal high
compare_cors(norm_h_H6_expl$full, unif_H6_expl$full)

# normal moderate
compare_cors(norm_m_H6_expl$full, unif_H6_expl$full)

# normal low
compare_cors(norm_l_H6_expl$full, unif_H6_expl$full)

## Comparisons within BARTnormal
# normal moderate vs normal high
compare_cors(norm_m_H6_expl$full, norm_h_H6_expl$full)

# normal low vs normal high
compare_cors(norm_l_H6_expl$full, norm_h_H6_expl$full)

# normal low vs normal moderate
compare_cors(norm_l_H6_expl$full, norm_m_H6_expl$full)


### compute all test-retest reliabilities and coefficient of variations ========

vars <- names(s2)[c(13:20, 27:32)]
reliabilities <- list()
for (i in seq_along(vars)) {

  dat_s1 <- s2[[paste0(vars[i], "_s1")]]
  dat_s2 <- s2[[vars[i]]]
  
  cor_i <- correlationBF(dat_s1, dat_s2, iterations = 10000)
  post_i <- describe_posterior(cor_i, rope_range = c(-.05, .05), ci = .95,
                               rope_ci = 1)
  
  cv_i <- cv(dat_s1, dat_s2)
  
  reliabilities[[i]] <- data.frame(
    variable = vars[i],
    reliability = post_i$Median,
    cv = cv_i
  )
  
}
reliabilities <- do.call(rbind, reliabilities)

# average reliabilities of frequency and propensity measures

reliabilities %>%
  mutate(type = case_when(grepl("rlrisk", variable) ~ "freq",
                          TRUE ~"prop")) %>%
  group_by(type) %>%
  summarise(
    mr = mean(reliability),
    mcv = mean(cv)
  )


reliabilities <- rbind(reliabilities,
                       data.frame(
                         variable = c("bel_unif", "bel_h", "bel_m", "bel_l",
                                      "adj_unif", "adj_h", "adj_m", "adj_l",
                                      "expl_unif", "expl_h", "expl_m", "expl_l"),
                         reliability = c(unif_H5$corr, norm_h_H5$corr, norm_m_H5$corr,
                                         norm_l_H5$corr, unif_H6$corr, norm_h_H6$corr,
                                         norm_m_H6$corr, norm_l_H6$corr, unif_H6_expl$corr,
                                         norm_h_H6_expl$corr, norm_m_H6_expl$corr,
                                         norm_l_H6_expl$corr),
                         cv = c(unif_H5$cv, norm_h_H5$cv, norm_m_H5$cv,
                                norm_l_H5$cv, unif_H6$cv, norm_h_H6$cv,
                                norm_m_H6$cv, norm_l_H6$cv, unif_H6_expl$cv,
                                norm_h_H6_expl$cv, norm_m_H6_expl$cv,
                                norm_l_H6_expl$cv)
                       ))


### Plot reliabilities and CVs

pdf("plots/s2/cv_reliabilities.pdf", height = 6, width = 9)
temp_rel <- reliabilities %>%
  mutate( mtype = case_when(grepl("rlrisk", variable) ~ "freq",
                      grepl("bel|adj|expl", variable) ~ "beh",
                      TRUE ~ "prop"),
    variable = case_when(variable == "grips_score" ~ "GRiPS",
                   variable == "soep_gen" ~ "SOEP[general]",
                   variable == "soep_driving" ~ "SOEP[driving]",
                   variable == "soep_finance" ~ "SOEP[finance]",
                   variable == "soep_health" ~ "SOEP[health]",
                   variable == "soep_leisure" ~ "SOEP[leisure]",
                   variable == "soep_occupation" ~ "SOEP[occupation]",
                   variable == "soep_faith" ~ "SOEP[social]",
                   variable == "rlrisk_drink_b" ~ "Drinking",
                   variable == "rlrisk_gamble_b" ~ "Gambling",
                   variable == "rlrisk_invest_b" ~ "Investing",
                   variable == "rlrisk_cigarettes_b" ~ "Smoking",
                   variable == "rlrisk_speed_b" ~ "Speeding",
                   variable == "rlrisk_sport_b" ~ "Sport",
                   variable == "premeditation" ~ "Premeditation",
                   variable == "perseverance" ~"Perseverance",
                   variable == "urgency" ~ "Urgency",
                   variable == "ss" ~ "Senation~Seeking",
                   variable == "bel_unif" ~ "B~BART[uniform]",
                   variable == "bel_h" ~ "B~BART[normal-H]",
                   variable == "bel_m" ~ "B~BART[normal-M]",
                   variable == "bel_l" ~ "B~BART[normal-L]",
                   variable == "adj_unif" ~ "AS~BART[uniform]",
                   variable == "adj_h" ~ "AS~BART[normal-H]",
                   variable == "adj_m" ~ "AS~BART[normal-M]",
                   variable == "adj_l" ~ "AS~BART[normal-L]",
                   variable == "expl_unif" ~ "NE~BART[uniform]",
                   variable == "expl_h" ~ "NE~BART[normal-H]",
                   variable == "expl_m" ~ "NE~BART[normal-M]",
                   variable == "expl_l" ~ "NE~BART[normal-L]")
  )

temp_rel %>%
  filter(mtype != "freq") %>%
  ggplot(aes(reliability, cv, label = variable, col = mtype)) +
  geom_vline(data = temp_rel %>% filter(mtype == "freq"),
              aes(xintercept = reliability), linetype = 2, size = 1,
             col = "#62c86c") +
  geom_point(size = 3.5, pch = 15) +
  geom_label_repel(parse = TRUE, show.legend = FALSE, size = 3.25, col = "black",
                   max.iter = 10000, ylim = c(0, 1.8), min.segment.length = .9,
                  point.padding = .1, label.size = NA) +
  geom_label_repel(data = temp_rel %>% filter(mtype == "freq"),
            aes(x = reliability, y = rep(0, 6)), parse = TRUE,
            show.legend = FALSE, size = 3.25, col = "black",
            max.iter = 10000, point.padding = .1, label.size = NA) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_color_manual(values = c("#2b908d", "#fbe94b")) +
  labs(
    x = "Test-Retest Reliability",
    y = "Coefficient of Variation"
  ) +
  theme_light() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12, color = "black"),
      legend.position = "none"
    )
dev.off()

