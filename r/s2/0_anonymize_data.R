# Script to delete the feedback column of participants data, as it may contain
# identifying information.

library(tidyverse)

participants <- read_csv("data/s2/participants.csv")

participants <- participants %>%
  select(-feedback)

write_csv(participants, "data/s2/participants.csv")
