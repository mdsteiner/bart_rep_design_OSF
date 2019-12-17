library(tidyverse)
library(viridis)
library(yarrr)
library(patchwork)
source("r/helper.R")


### Reanalysis Study 1 Schürmann, Frey, and Pleskac (2018) =====================
load("data/reanalyses/SchuermannEtAl2018/st1/BART_perception.RData")

# dat_schurmann_s1 <- BART.perc.data %>%
#   mutate(rating = rating / 100) %>%
#   group_by(partID) %>%
#   summarise(mse_norm = fit_norm(size, rating),
#             m_norm = fit_norm(size, rating, out = "par")[1],
#             sd_norm = fit_norm(size, rating, out = "par")[2],
#             mse_norm_cond = fit_norm(size, rating, dist = "conditional"),
#             m_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional")[1],
#             sd_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional")[2],
#             mse_unif = fit_unif(size, rating),
#             min_unif = fit_unif(size, rating, out = "par")[1],
#             max_unif = fit_unif(size, rating, out = "par")[2],
#             mse_unif_cond = fit_unif(size, rating, dist = "conditional"),
#             min_unif_cond = fit_unif(size, rating, out = "par", dist = "conditional")[1],
#             best_model = c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")[which.min(c(mse_norm, mse_norm_cond, mse_unif, mse_unif_cond))],
#             es_norm_cdf = (1 - mse_norm) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_norm_cond = (1 - mse_norm_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cdf = (1 - mse_unif) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cond = (1 - mse_unif_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))))
# 
# saveRDS(dat_schurmann_s1, "data/reanalysis_Schuermann2018_s1.RDS")

dat_schurmann_s1 <- readRDS("data/reanalyses/reanalysis_Schuermann2018_s1.RDS")


### Reanalysis Study 2 Schürmann, Frey, and Pleskac (2018) =====================
load("data/reanalyses/SchuermannEtAl2018/st1/perception2.RData")

# dat_schurmann_s2 <- perception2.data %>%
#   mutate(rating1 = rating1 / 100,
#          rating2 = rating2 / 100) %>%
#   group_by(partID) %>%
#   summarise(mse_norm_1 = fit_norm(size, rating1),
#             m_norm_1 = fit_norm(size, rating1, out = "par")[1],
#             sd_norm_1 = fit_norm(size, rating1, out = "par")[2],
#             mse_norm_cond_1 = fit_norm(size, rating1, dist = "conditional"),
#             m_norm_cond_1 = fit_norm(size, rating1, out = "par", dist = "conditional")[1],
#             sd_norm_cond_1 = fit_norm(size, rating1, out = "par", dist = "conditional")[2],
#             mse_unif_1 = fit_unif(size, rating1),
#             min_unif_1 = fit_unif(size, rating1, out = "par")[1],
#             max_unif_1 = fit_unif(size, rating1, out = "par")[2],
#             mse_unif_cond_1 = fit_unif(size, rating1, dist = "conditional"),
#             min_unif_cond_1 = fit_unif(size, rating1, out = "par", dist = "conditional")[1],
#             best_model_1 = c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")[which.min(c(mse_norm_1, mse_norm_cond_1, mse_unif_1, mse_unif_cond_1))],
#             es_norm_cdf_1 = (1 - mse_norm_1) / sum(c((1 - mse_norm_1), (1 -mse_norm_cond_1), (1 - mse_unif_1), (1 -mse_unif_cond_1))),
#             es_norm_cond_1 = (1 - mse_norm_cond_1) / sum(c((1 - mse_norm_1), (1 -mse_norm_cond_1), (1 - mse_unif_1), (1 -mse_unif_cond_1))),
#             es_unif_cdf_1 = (1 - mse_unif_1) / sum(c((1 - mse_norm_1), (1 -mse_norm_cond_1), (1 - mse_unif_1), (1 -mse_unif_cond_1))),
#             es_unif_cond_1 = (1 - mse_unif_cond_1) / sum(c((1 - mse_norm_1), (1 -mse_norm_cond_1), (1 - mse_unif_1), (1 -mse_unif_cond_1))),
# 
#             mse_norm_2 = fit_norm(size, rating2),
#             m_norm_2 = fit_norm(size, rating2, out = "par")[1],
#             sd_norm_2 = fit_norm(size, rating2, out = "par")[2],
#             mse_norm_cond_2 = fit_norm(size, rating2, dist = "conditional"),
#             m_norm_cond_2 = fit_norm(size, rating2, out = "par", dist = "conditional")[1],
#             sd_norm_cond_2 = fit_norm(size, rating2, out = "par", dist = "conditional")[2],
#             mse_unif_2 = fit_unif(size, rating2),
#             min_unif_2 = fit_unif(size, rating2, out = "par")[1],
#             max_unif_2 = fit_unif(size, rating2, out = "par")[2],
#             mse_unif_cond_2 = fit_unif(size, rating2, dist = "conditional"),
#             min_unif_cond_2 = fit_unif(size, rating2, out = "par", dist = "conditional")[1],
#             best_model_2 = c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")[which.min(c(mse_norm_2, mse_norm_cond_2, mse_unif_2, mse_unif_cond_2))],
#             es_norm_cdf_2 = (1 - mse_norm_2) / sum(c((1 - mse_norm_2), (1 -mse_norm_cond_2), (1 - mse_unif_2), (1 -mse_unif_cond_2))),
#             es_norm_cond_2 = (1 - mse_norm_cond_2) / sum(c((1 - mse_norm_2), (1 -mse_norm_cond_2), (1 - mse_unif_2), (1 -mse_unif_cond_2))),
#             es_unif_cdf_2 = (1 - mse_unif_2) / sum(c((1 - mse_norm_2), (1 -mse_norm_cond_2), (1 - mse_unif_2), (1 -mse_unif_cond_2))),
#             es_unif_cond_2 = (1 - mse_unif_cond_2) / sum(c((1 - mse_norm_2), (1 -mse_norm_cond_2), (1 - mse_unif_2), (1 -mse_unif_cond_2))))
# 
# saveRDS(dat_schurmann_s2, "data/reanalysis_Schuermann2018_s2.RDS")

dat_schurmann_s2 <- readRDS("data/reanalyses/reanalysis_Schuermann2018_s2.RDS")


### Reanalysis Frey et al. (2018) ==============================================

perc_dat <- read_csv("data/reanalyses/FreyEtAl2017/bart_riskperc.csv")

# dat_frey <- perc_dat %>%
#   mutate(rating = rating / 100) %>%
#   group_by(partid) %>%
#   summarise(mse_norm = fit_norm(size, rating),
#             m_norm = fit_norm(size, rating, out = "par")[1],
#             sd_norm = fit_norm(size, rating, out = "par")[2],
#             mse_norm_cond = fit_norm(size, rating, dist = "conditional"),
#             m_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional")[1],
#             sd_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional")[2],
#             mse_unif = fit_unif(size, rating),
#             min_unif = fit_unif(size, rating, out = "par")[1],
#             max_unif = fit_unif(size, rating, out = "par")[2],
#             mse_unif_cond = fit_unif(size, rating, dist = "conditional"),
#             min_unif_cond = fit_unif(size, rating, out = "par", dist = "conditional")[1],
#             best_model = c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")[which.min(c(mse_norm, mse_norm_cond, mse_unif, mse_unif_cond))],
#             es_norm_cdf = (1 - mse_norm) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_norm_cond = (1 - mse_norm_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cdf = (1 - mse_unif) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cond = (1 - mse_unif_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))))
# 
# saveRDS(dat_frey, "data/reanalysis_Frey2017.RDS")

dat_frey <- readRDS("data/reanalyses/reanalysis_Frey2017.RDS")

### Reanalysis Pilot Steiner and Frey (2019) ===================================

raw <- read_csv("data/reanalyses/RiskprocPilot/bart_riskperc.csv")
dist_rating <- read_csv("data/reanalyses/RiskprocPilot/bartassess.csv")
bart <- read_csv("data/reanalyses/RiskprocPilot/bart_results.csv")
soep <- read_csv("data/reanalyses/RiskprocPilot/soep_results.csv")

# proportion of participants who thought explosion points were rather in line
# with normal and uniform distribution, respectively
prop.table(table(dist_rating$distribution_rating))

# dat_steiner <- raw %>%
#   mutate(rating = rating / 100) %>%
#   group_by(partid) %>%
#   summarise(mse_norm = fit_norm(size, rating),
#             m_norm = fit_norm(size, rating, out = "par")[1],
#             sd_norm = fit_norm(size, rating, out = "par")[2],
#             mse_norm_cond = fit_norm(size, rating, dist = "conditional", max_p = 64),
#             m_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional", max_p = 64)[1],
#             sd_norm_cond = fit_norm(size, rating, out = "par", dist = "conditional", max_p = 64)[2],
#             mse_unif = fit_unif(size, rating),
#             min_unif = fit_unif(size, rating, out = "par")[1],
#             max_unif = fit_unif(size, rating, out = "par")[2],
#             mse_unif_cond = fit_unif(size, rating, dist = "conditional", max_p = 64),
#             min_unif_cond = fit_unif(size, rating, out = "par", dist = "conditional", max_p = 64)[1],
#             best_model = c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")[which.min(c(mse_norm, mse_norm_cond, mse_unif, mse_unif_cond))],
#             es_norm_cdf = (1 - mse_norm) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_norm_cond = (1 - mse_norm_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cdf = (1 - mse_unif) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))),
#             es_unif_cond = (1 - mse_unif_cond) / sum(c((1 - mse_norm), (1 -mse_norm_cond), (1 - mse_unif), (1 -mse_unif_cond))))
# 
# saveRDS(dat_steiner, "data/reanalysis_SteinerFreyPilot.RDS")

dat_steiner <- readRDS("data/reanalyses/reanalysis_SteinerFreyPilot.RDS")

### Tables of best fitting models ==============================================
prop.table(table(dat_schurmann_s1$best_model))
prop.table(table(dat_schurmann_s2$best_model_1))
prop.table(table(dat_schurmann_s2$best_model_2))
prop.table(table(dat_frey$best_model))
prop.table(table(dat_steiner$best_model))

### Setup plot curves ============================================================

pdf("plots/reanalyses.pdf", height = 6, width = 10)

layout(matrix(c(1, 1, 2, 2, 3, 3,
                4, 4, 5, 5, 6, 6), byrow = TRUE, ncol = 6))
par(mar = c(4, 7, 3, 1))

cols <- inferno(4, end = .8,  alpha = .8)
names(cols) <- c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")

pchv <- c(16, 15, 17, 18)
names(pchv) <- c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")

### Plot curves Study 1 Schürmann, Frey, and Pleskac. (2018) -------------------

ids <- dat_schurmann_s1 %>% filter(best_model == "norm_cdf") %>% select(partID) %>% pull()

plot(BART.perc.data$size[BART.perc.data$partID %in% ids],
     BART.perc.data$rating[BART.perc.data$partID %in% ids] / 100,
     ylab = "Probability Rating", xlab = "Balloon Size", ylim = c(0, 1),
     xlim = c(0, 128), col = cols[1], pch = 16, las = 1, cex.axis = 1.2,
     cex.lab = 1.5, bty = "l")

mtext(paste0("Normal CDF best: ", round(mean(dat_schurmann_s1$best_model == "norm_cdf"),
                                          2) * 100, "%"), cex = 1.1)
text("a)", font = 2, xpd = TRUE, x = -40, y = 1, cex = 2)

for (best_mod in c("norm_cond", "unif_cdf", "unif_cond")) {
  ids <- dat_schurmann_s1 %>% filter(best_model == best_mod) %>% select(partID) %>% pull()
  points(BART.perc.data$size[BART.perc.data$partID %in% ids],
         BART.perc.data$rating[BART.perc.data$partID %in% ids] / 100,
         col = cols[best_mod], pch = pchv[best_mod])
}


# plot NCDFs
temp <- dat_schurmann_s1 %>% filter(best_model == "norm_cdf")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          pnorm(seq(1, 128, .5), temp$m_norm[part_i], temp$sd_norm[part_i]),
          col = cols["norm_cdf"])
    
  }
}


# plot N conditional
temp <- dat_schurmann_s1 %>% filter(best_model == "norm_cond")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    m_i <- temp$m_norm_cond[part_i]
    sd_i <- temp$sd_norm_cond[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dtruncnorm((pump + 1), mean = m_i, sd = sd_i, a = -Inf, b = 128) /
                  sum(dtruncnorm((pump + 1):128, mean = m_i, sd = sd_i, a = -Inf, b = 128)))
    }
    
    lines(1:128, pred, col = cols["norm_cond"])
    
  }
}


# plot UCDFs
temp <- dat_schurmann_s1 %>% filter(best_model == "unif_cdf")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          punif(seq(1, 128, .5), temp$min_unif[part_i], temp$max_unif[part_i]),
          col = cols["unif_cdf"], lwd = 1)
    
  }
}


# plot conditional
temp <- dat_schurmann_s1 %>% filter(best_model == "unif_cond")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    min_i <- temp$min_unif_cond[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dunif((pump + 1), min = min_i, max = 128) /
                  sum(dunif((pump + 1):128, min = min_i, max = 128)))
    }
    
    lines(1:128, pred, col = cols["unif_cond"])
    
  }
  
}


### Plot curves Study 2 Schürmann, Frey, and Pleskac. (2018) -------------------
### Plot Timepoint 1

ids <- dat_schurmann_s2 %>% filter(best_model_1 == "norm_cdf") %>% select(partID) %>% pull()

plot(perception2.data$size[perception2.data$partID %in% ids],
     perception2.data$rating1[perception2.data$partID %in% ids] / 100,
     ylab = "Probability Rating", xlab = "Balloon Size", ylim = c(0, 1),
     xlim = c(0, 128), col = cols[1], pch = 16, las = 1, cex.axis = 1.2,
     cex.lab = 1.5, bty = "l")

mtext(paste0("Normal CDF best: ", round(mean(dat_schurmann_s2$best_model_1 == "norm_cdf"),
                                          2) * 100, "%"), cex = 1.1)
text("b)", font = 2, xpd = TRUE, x = -40, y = 1, cex = 2)

for (best_mod in c("norm_cond", "unif_cdf", "unif_cond")) {
  ids <- dat_schurmann_s2 %>% filter(best_model_1 == best_mod) %>% select(partID) %>% pull()
  points(perception2.data$size[perception2.data$partID %in% ids],
         perception2.data$rating1[perception2.data$partID %in% ids] / 100,
         col = cols[best_mod], pch = pchv[best_mod])
}


# plot NCDFs
temp <- dat_schurmann_s2 %>% filter(best_model_1 == "norm_cdf")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          pnorm(seq(1, 128, .5), temp$m_norm_1[part_i], temp$sd_norm_1[part_i]),
          col = cols["norm_cdf"])
    
  }
}


# plot N conditional
temp <- dat_schurmann_s2 %>% filter(best_model_1 == "norm_cond")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    m_i <- temp$m_norm_cond_1[part_i]
    sd_i <- temp$sd_norm_cond_1[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dtruncnorm((pump + 1), mean = m_i, sd = sd_i, a = -Inf, b = 128) /
                  sum(dtruncnorm((pump + 1):128, mean = m_i, sd = sd_i, a = -Inf, b = 128)))
    }
    
    lines(1:128, pred, col = cols["norm_cond"])
    
  }
}


# plot UCDFs
temp <- dat_schurmann_s2 %>% filter(best_model_1 == "unif_cdf")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          punif(seq(1, 128, .5), temp$min_unif_1[part_i], temp$max_unif_1[part_i]),
          col = cols["unif_cdf"], lwd = 1)
    
  }
}


# plot conditional
temp <- dat_schurmann_s2 %>% filter(best_model_1 == "unif_cond")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    min_i <- temp$min_unif_cond_1[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dunif((pump + 1), min = min_i, max = 128) /
                  sum(dunif((pump + 1):128, min = min_i, max = 128)))
    }
    
    lines(1:128, pred, col = cols["unif_cond"])
    
  }
  
}


### Plot Timepoint 2


ids <- dat_schurmann_s2 %>% filter(best_model_2 == "norm_cdf") %>% select(partID) %>% pull()

plot(perception2.data$size[perception2.data$partID %in% ids],
     perception2.data$rating2[perception2.data$partID %in% ids] / 100,
     ylab = "Probability Rating", xlab = "Balloon Size", ylim = c(0, 1),
     xlim = c(0, 128), col = cols[1], pch = 16, las = 1, cex.axis = 1.2,
     cex.lab = 1.5, bty = "l")

mtext(paste0("Normal CDF best: ", round(mean(dat_schurmann_s2$best_model_2 == "norm_cdf"),
                                          2) * 100, "%"), cex = 1.1)
text("c)", font = 2, xpd = TRUE, x = -40, y = 1, cex = 2)

for (best_mod in c("norm_cond", "unif_cdf", "unif_cond")) {
  ids <- dat_schurmann_s2 %>% filter(best_model_2 == best_mod) %>% select(partID) %>% pull()
  points(perception2.data$size[perception2.data$partID %in% ids],
         perception2.data$rating2[perception2.data$partID %in% ids] / 100,
         col = cols[best_mod], pch = pchv[best_mod])
}


# plot NCDFs
temp <- dat_schurmann_s2 %>% filter(best_model_2 == "norm_cdf")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          pnorm(seq(1, 128, .5), temp$m_norm_2[part_i], temp$sd_norm_2[part_i]),
          col = cols["norm_cdf"])
    
  }
}


# plot N conditional
temp <- dat_schurmann_s2 %>% filter(best_model_2 == "norm_cond")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    m_i <- temp$m_norm_cond_2[part_i]
    sd_i <- temp$sd_norm_cond_2[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dtruncnorm((pump + 1), mean = m_i, sd = sd_i, a = -Inf, b = 128) /
                  sum(dtruncnorm((pump + 1):128, mean = m_i, sd = sd_i, a = -Inf, b = 128)))
    }
    
    lines(1:128, pred, col = cols["norm_cond"])
    
  }
}


# plot UCDFs
temp <- dat_schurmann_s2 %>% filter(best_model_2 == "unif_cdf")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          punif(seq(1, 128, .5), temp$min_unif_2[part_i], temp$max_unif_2[part_i]),
          col = cols["unif_cdf"], lwd = 1)
    
  }
}


# plot conditional
temp <- dat_schurmann_s2 %>% filter(best_model_2 == "unif_cond")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    min_i <- temp$min_unif_cond_2[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dunif((pump + 1), min = min_i, max = 128) /
                  sum(dunif((pump + 1):128, min = min_i, max = 128)))
    }
    
    lines(1:128, pred, col = cols["unif_cond"])
    
  }
  
}

### Plot curves Frey et al. (2017) ---------------------------------------------

cols <- inferno(4, end = .8,  alpha = .2)
names(cols) <- c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")

pchv <- c(16, 15, 17, 18)
names(pchv) <- c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")

ids <- dat_frey %>% filter(best_model == "norm_cdf") %>% select(partid) %>% pull()

plot(perc_dat$size[perc_dat$partid %in% ids],
     perc_dat$rating[perc_dat$partid %in% ids] / 100,
     ylab = "Probability Rating", xlab = "Balloon Size", ylim = c(0, 1),
     xlim = c(0, 128), col = cols[1], pch = 16, las = 1, cex.axis = 1.2,
     cex.lab = 1.5, bty = "l")

mtext(paste0("Normal CDF best: ", round(mean(dat_frey$best_model == "norm_cdf"),
                                      2) * 100, "%"), cex = 1.1)
text("d)", font = 2, xpd = TRUE, x = -40, y = 1, cex = 2)

for (best_mod in c("norm_cond", "unif_cdf", "unif_cond")) {
  ids <- dat_frey %>% filter(best_model == best_mod) %>% select(partid) %>% pull()
  points(perc_dat$size[perc_dat$partid %in% ids],
         perc_dat$rating[perc_dat$partid %in% ids] / 100,
         col = cols[best_mod], pch = pchv[best_mod])
}


# plot NCDFs
temp <- dat_frey %>% filter(best_model == "norm_cdf")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          pnorm(seq(1, 128, .5), temp$m_norm[part_i], temp$sd_norm[part_i]),
          col = cols["norm_cdf"])
    
  }
}


# plot N conditional
temp <- dat_frey %>% filter(best_model == "norm_cond")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    m_i <- temp$m_norm_cond[part_i]
    sd_i <- temp$sd_norm_cond[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dtruncnorm((pump + 1), mean = m_i, sd = sd_i, a = -Inf, b = 128) /
                  sum(dtruncnorm((pump + 1):128, mean = m_i, sd = sd_i, a = -Inf, b = 128)))
    }
    
    lines(1:128, pred, col = cols["norm_cond"])
    
  }
}


# plot UCDFs
temp <- dat_frey %>% filter(best_model == "unif_cdf")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, 128, .5),
          punif(seq(1, 128, .5), temp$min_unif[part_i], temp$max_unif[part_i]),
          col = cols["unif_cdf"], lwd = 1)
    
  }
}


# plot conditional
temp <- dat_frey %>% filter(best_model == "unif_cond")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    min_i <- temp$min_unif_cond[part_i]
    pred <- NULL
    for(pump in 0:127) {
      
      pred <- c(pred,
                dunif((pump + 1), min = min_i, max = 128) /
                  sum(dunif((pump + 1):128, min = min_i, max = 128)))
    }
    
    lines(1:128, pred, col = cols["unif_cond"])
    
  }
  
}

cols <- inferno(4, end = .8,  alpha = .8)
names(cols) <- c("norm_cdf", "norm_cond", "unif_cdf", "unif_cond")


### Plot curves Steiner and Frey (2019) ----------------------------------------

max_p <- 64

ids <- dat_steiner %>% filter(best_model == "norm_cdf") %>% select(partid) %>% pull()

plot(raw$size[raw$partid %in% ids],
     raw$rating[raw$partid %in% ids] / 100,
     ylab = "Probability Rating", xlab = "Balloon Size", ylim = c(0, 1),
     xlim = c(0, max_p), col = cols[1], pch = 16, las = 1, cex.axis = 1.2,
     cex.lab = 1.5, bty = "l")

mtext(paste0("Normal CDF best: ", round(mean(dat_steiner$best_model == "norm_cdf"),
                                          2) * 100, "%"), cex = 1.1)
text("e)", font = 2, xpd = TRUE, x = -20, y = 1, cex = 2)

for (best_mod in c("norm_cond", "unif_cdf", "unif_cond")) {
  ids <- dat_steiner %>% filter(best_model == best_mod) %>% select(partid) %>% pull()
  points(raw$size[raw$partid %in% ids],
         raw$rating[raw$partid %in% ids] / 100,
         col = cols[best_mod], pch = pchv[best_mod])
}


# plot NCDFs
temp <- dat_steiner %>% filter(best_model == "norm_cdf")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, max_p, .5),
          pnorm(seq(1, max_p, .5), temp$m_norm[part_i], temp$sd_norm[part_i]),
          col = cols["norm_cdf"])
    
  }
}


# plot N conditional
temp <- dat_steiner %>% filter(best_model == "norm_cond")

if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    m_i <- temp$m_norm_cond[part_i]
    sd_i <- temp$sd_norm_cond[part_i]
    pred <- NULL
    for(pump in 0:(max_p - 1)) {
      
      pred <- c(pred,
                dtruncnorm((pump + 1), mean = m_i, sd = sd_i, a = -Inf, b = max_p) /
                  sum(dtruncnorm((pump + 1):max_p, mean = m_i, sd = sd_i, a = -Inf, b = max_p)))
    }
    
    lines(1:max_p, pred, col = cols["norm_cond"])
    
  }
}


# plot UCDFs
temp <- dat_steiner %>% filter(best_model == "unif_cdf")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    lines(seq(1, max_p, .5),
          punif(seq(1, max_p, .5), temp$min_unif[part_i], temp$max_unif[part_i]),
          col = cols["unif_cdf"], lwd = 1)
    
  }
}


# plot conditional
temp <- dat_steiner %>% filter(best_model == "unif_cond")
if (nrow(temp) > 0) {
  for (part_i in 1:nrow(temp)) {
    
    min_i <- temp$min_unif_cond[part_i]
    pred <- NULL
    for(pump in 0:(max_p - 1)) {
      
      pred <- c(pred,
                dunif((pump + 1), min = min_i, max = max_p) /
                  sum(dunif((pump + 1):max_p, min = min_i, max = max_p)))
    }
    
    lines(1:max_p, pred, col = cols["unif_cond"])
    
  }
  
}


### Plot legend ================================================================

h_adj <- .2

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
points(rep(.1, 4) + h_adj, seq(.2, .8, length.out = 4), pch = pchv, col = cols,
       cex = 2.4)
text(x = rep(.1, 4) + .1 + h_adj, y = seq(.2, .8, length.out = 4), cex = 1.4,
     labels = c("Normal CDF", "Normal CPF", "Uniform CDF", "Uniform CPF"), offset = 0,
     adj = 0)



dev.off()
