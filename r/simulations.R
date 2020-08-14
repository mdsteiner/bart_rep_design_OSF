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
source("r/s1/functions.R")
library(retimes)
library(truncnorm)

s1 <- readRDS("data/s1/study_1.RDS")
s2 <- readRDS("data/s2/study_2.RDS")

dists <- readRDS("data/experiment/bart_distributions.RDS")


### Check for differences in n explosions and reliabilities between unif and norm

# are number of explosions different across conditions
s1 %>%
  group_by(cond_dist) %>%
  summarise(m = mean(n_explosions),
            sd = sd(n_explosions))
s2 %>%
  group_by(cond_dist) %>%
  summarise(m = mean(n_explosions),
            sd = sd(n_explosions))

# compare reliabilities

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


# -> based on the empirical findings, there does not seem to be conclusive
#    evidence for a difference in either N explosion nor in the reliabilities

### Simulation analyses ========================================================

# Simulation analyses to test the purely statistical effect of the different 
# distributions on the differences in the respective adjusted BART scores

# Model definitions ------------------------------------------------------------
# 1) basic model with a stationary target

stat_model <- function(e, n_part, which_dist, sd_1, dist) {
  expl_i <- rep(dist[[which_dist]], n_part)
  set.seed(42)
  aspired <- round(rtruncnorm(30 * n_part, mean = e, sd = sd_1,
                              a = .5, b = Inf))
  exploded <- aspired >= expl_i
  n_pumps <- ifelse(exploded, expl_i, aspired)
  
  dat <- tibble(
    id = rep(1:n_part, each = 30),
    trial = rep(1:30, n_part),
    explosion_points = expl_i,
    exploded = as.numeric(exploded),
    n_pumps = n_pumps,
    expectation = e,
    dist = which_dist,
    sd_1 = sd_1,
    target = e
  )
  
  return(dat)
  
}

# Run simulations (only once needed) -------------------------------------------


model_control <- expand.grid(
  target = c(1, seq(8, 64, 8)),
  sd_target = c(0, 5, 10),
  dists = names(dists),
  stringsAsFactors = FALSE
)

res_stat <- list()

for (i in 1:nrow(model_control)) {

  res_stat[[i]] <- stat_model(model_control$target[i], 1000,
                              model_control$dists[i],
                              model_control$sd_target[i], dists)

}


res_stat <- do.call(rbind, res_stat)
res_stat$model = "static"
saveRDS(res_stat, file = "data/static_model_raw.RDS")

res <- res_stat %>% 
  group_by(id, dist, target, sd_1, model) %>% 
  summarise(
    adj_bart_score = mean(n_pumps[exploded != 1]),
    bart_score = mean(n_pumps),
    n_explosions = sum(exploded)
  ) %>% 
  ungroup()

saveRDS(res, file = "data/simulation_models.RDS")
rm(res, res_stat)


# Analyse simulation results ---------------------------------------------------

sim_dat_agg <- readRDS("data/simulation_models.RDS")

# plot adjusted BART scores

sim_plot <- function(sim_dat_agg, mod, yax, yvar, lim) {
  p <- sim_dat_agg %>% 
    filter(model == mod) %>% 
    mutate(target = factor(target),
           dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low")),
           sd_1 = factor(sd_1)) %>% 
    ggplot(aes_string("target", yvar, fill = "dist")) +
    geom_violin(na.rm = TRUE) +
    stat_summary(fun = mean, geom = "point", size = 1.5, color = "grey",
                 position = position_dodge(.9), na.rm = TRUE, show.legend = FALSE) +
    #  geom_jitter(alpha = .1, position = position_dodge(.9)) +
    facet_grid(sd_1 ~.) +
    scale_fill_manual(name = "Distribution", values = inferno(4), 
                      labels = c(expression(BART[uniform]), expression(BART[normal-H]),
                                 expression(BART[normal-M]), expression(BART[normal-L]))) +
    labs(x = "Target", y = yax) +
    coord_cartesian(ylim = lim) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(vjust = 0, size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(hjust = 0),
      strip.text = element_text(size = 12)
    )
  
  print(p)
}

sim_plot2 <- function(sim_dat_agg, mod, yax, yvar, lim, sd) {
  p <- sim_dat_agg %>% 
    filter(model == mod, sd_1 == sd) %>% 
    mutate(target = factor(target),
           dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low"))) %>% 
    ggplot(aes_string("target", yvar, fill = "dist")) +
    geom_violin(na.rm = TRUE) +
    stat_summary(fun = mean, geom = "point", size = 1.5, color = "grey",
                 position = position_dodge(.9), na.rm = TRUE, show.legend = FALSE) +
    #  geom_jitter(alpha = .1, position = position_dodge(.9)) +
    scale_fill_manual(name = "Distribution", values = inferno(4), 
                      labels = c(expression(BART[uniform]), expression(BART[normal-H]),
                                 expression(BART[normal-M]), expression(BART[normal-L]))) +
    labs(x = "Target", y = yax) +
    coord_cartesian(ylim = lim) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(vjust = 0, size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(hjust = 0),
      strip.text = element_text(size = 12)
    )
  
  print(p)
}


# plot adjusted BART scores
pdf("plots/simulations/static_model.pdf", height = 8, width = 12)
sim_plot(sim_dat_agg, "static", "adjusted BART scores | static model",
         "adj_bart_score", c(1, 64))
dev.off()

# Plot number of explosions
pdf("plots/simulations/static_model_n_expl.pdf", height = 8, width = 12)
sim_plot(sim_dat_agg, "static", "n explosions | static model",
         "n_explosions", c(1, 30))
dev.off()


# Plot function for 2 by 2 plots with bart scores vs n explosions
plot_fun2 <- function(sim_dat_agg, mod, sd_i, modt){
  temp_cor <- sim_dat_agg %>% 
    filter(model == mod, sd_1 == sd_i, target %in% c(8, 32, 56)) %>% 
    mutate(dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low"),
                         labels = c("BART[uniform]", "BART[normal-H]",
                                    "BART[normal-M]", "BART[normal-L]"))) %>% 
    group_by(dist) %>% 
    summarise(
      rs = round(cor(n_explosions, adj_bart_score, method = "spearman",
                     use = "pairwise"), 2)
    )
  
  p <- sim_dat_agg %>% 
    filter(model == mod, sd_1 == sd_i, target %in% c(8, 32, 56)) %>% 
    mutate(dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low"),
                         labels = c("BART[uniform]", "BART[normal-H]",
                                    "BART[normal-M]", "BART[normal-L]")),
           target = factor(target)) %>% 
    ggplot(aes(adj_bart_score, n_explosions)) +
    geom_jitter(aes(col = target), na.rm = TRUE, alpha = .1, width = .2) +
    geom_smooth(method = "lm") +
    geom_label(data = temp_cor, aes(x = 5, y = 27, group = 1,
                                    label = paste0("italic(r[s])==", rs)),
               parse = TRUE, size = 4, fill = "white") +
    #  geom_jitter(alpha = .1, position = position_dodge(.9)) +
    facet_wrap(~dist, labeller = label_bquote(.(dist))) +
    labs(x = "adjusted BART score", y = "n explosions", subtitle = modt) +
    coord_cartesian(ylim = c(1, 30), xlim = c(1, 64)) +
    scale_color_manual(values = viridis(3)) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(vjust = 0, size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(hjust = 0),
      strip.text = element_text(size = 12),
      legend.position = "none"
    )
  print(p)
}



# static model
pdf("plots/simulations/score_vs_expl_static_model_2x2_sd0.pdf", height = 9, width = 9)
plot_fun2(sim_dat_agg, "static", 0, "static model")
dev.off()

pdf("plots/simulations/score_vs_expl_static_model_2x2_sd5.pdf", height = 9, width = 9)
plot_fun2(sim_dat_agg, "static", 5, "static model")
dev.off()

pdf("plots/simulations/score_vs_expl_static_model_2x2_sd10.pdf", height = 9, width = 9)
plot_fun2(sim_dat_agg, "static", 10, "static model")
dev.off()


# Plot function for 2 by 2 plots with bart scores, adj bart scores, and n expl
plot_fun3 <- function(sim_dat_agg, mod, sd_i, yax){
  temp_cor <- sim_dat_agg %>% 
    filter(model == mod, sd_1 == sd_i) %>% 
    mutate(dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low"),
                         labels = c("BART[uniform]", "BART[normal-H]",
                                    "BART[normal-M]", "BART[normal-L]"))) %>% 
    group_by(dist) %>% 
    summarise(
      rs = round(cor(target, adj_bart_score, method = "spearman",
                     use = "pairwise"), 2)
    )
  
  colors <- inferno(3, end = .8)
  names(colors) <- c("Adjusted BART scores", "BART scores", "Number of explosions")
  
  p <- sim_dat_agg %>% 
    filter(model == mod, sd_1 == sd_i) %>% 
    mutate(dist = factor(dist, levels = c("uniform", "normal_high",
                                          "normal_moderate", "normal_low"),
                         labels = c("BART[uniform]", "BART[normal-H]",
                                    "BART[normal-M]", "BART[normal-L]"))) %>% 
    ggplot(aes(target, adj_bart_score)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, col = "darkgray") +
    geom_segment(aes(x = 0, y = 15, xend = 32, yend = 15), lty = 3, col = colors[3],
                 alpha = .8, size = 1.5) +
    geom_segment(aes(x = 32, y = 0, xend = 32, yend = 15), lty = 3, col = colors[3],
                 alpha = .8, size = 1.5) +
    geom_point(aes(col = "Adjusted BART scores"), na.rm = TRUE) +
    geom_line(data = . %>% group_by(target, dist) %>% summarise(adj_bart_score = mean(adj_bart_score, na.rm = TRUE)), aes(col = "Adjusted BART scores"), na.rm = TRUE) +
    geom_point(aes(x = target, y = bart_score, col = "BART scores")) +
    geom_line(data = . %>% group_by(target, dist) %>% summarise(bart_score = mean(bart_score, na.rm = TRUE)), aes(x = target, y = bart_score, col = "BART scores"), na.rm = TRUE) +
    geom_point(aes(x = target, y = n_explosions, col = "Number of explosions")) +
    geom_line(data = . %>% group_by(target, dist) %>% summarise(n_explosions = mean(n_explosions, na.rm = TRUE)), aes(x = target, y = n_explosions, col = "Number of explosions"), na.rm = TRUE) +
    # geom_label(data = temp_cor, aes(x = 5, y = 58, group = 1,
    #                                 label = paste0("italic(r[s])==", rs)),
    #            parse = TRUE, size = 4, fill = "white") +
    #  geom_jitter(alpha = .1, position = position_dodge(.9)) +
    facet_wrap(~dist, labeller = label_parsed) +
    labs(x = "Target", y = yax, color = "Scores") +
    scale_color_manual(values = colors) +
    scale_y_continuous(breaks = c(1, seq(8, 64, 8))) + 
    scale_x_continuous(breaks = c(1, seq(8, 64, 8))) + 
    coord_cartesian(ylim = c(1, 64), xlim = c(1, 64)) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(vjust = 0, size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(hjust = 0, size = 12),
      strip.text = element_text(size = 12)
    )
  print(p)
}



# static model
pdf("plots/simulations/all_scores_static_model_2x2_sd0.pdf", height = 9, width = 12)
plot_fun3(sim_dat_agg, "static", 0, "Scores")
dev.off()

pdf("plots/simulations/all_scores_static_model_2x2_sd5.pdf", height = 9, width = 12)
plot_fun3(sim_dat_agg, "static", 5, "Scores")
dev.off()

pdf("plots/simulations/all_scores_static_model_2x2_sd10.pdf", height = 9, width = 12)
plot_fun3(sim_dat_agg, "static", 10, "Scores")
dev.off()