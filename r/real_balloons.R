library(tidyverse)

dat <- read_csv2("data/real_balloon_explosions.csv")

summary(dat$Explosion)
sd(dat$Explosion)

pdf("plots/distribution_real_balloons.pdf", height = 4, width = 6)
ggplot(dat, aes(Explosion)) +
  geom_histogram(binwidth = 1, col = "white") +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 12.5)) +
  scale_y_continuous(breaks = seq(0, 13, 2)) +
  labs(
    x = "Explosion Points",
    y = "Frequency"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")
  )
dev.off()

# only data of first package (first 87 balloons)
dat %>%
  slice(1:87) %>%
  ggplot(aes(Explosion)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Explosion Points",
    y = "Frequency"
  ) +
  theme_bw()
