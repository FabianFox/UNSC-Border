# Military interstate disputes data 4.3 (MID 4.3)

# Notes & Issues:
# - Check whether starting date >=1945 is reasonable

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio)

# Load MIDA 4.3
### ------------------------------------------------------------------------ ###
# One record per dispute
mid_inc.df <- import("./data/independent variables/MID 4.3/MIDA 4.3.csv") %>%
  filter(styear >= 1945)

# One record per participant per dispute
mid_dyad.df <- import("./data/independent variables/MID 4.3/MIDB 4.3.csv") %>%
  filter(styear >= 1945)

# Number of MIDs since 1945
mid.df %>%
  filter(styear >= 1945) %>%
  count(styear) %>%
  ggplot(aes(styear, n)) +
  geom_bar(stat = "identity")
