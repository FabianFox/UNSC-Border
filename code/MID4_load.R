# Military interstate disputes data 4.3 (MID 4.3)

# Notes & Issues:
# - Check whether starting date >=1945 is reasonable

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, countrycode)

# Load MID 4.3: A (incident level) and B (dyad level)
### ------------------------------------------------------------------------ ###
# One record per dispute
mid_inc.df <- import("./data/independent variables/MID 4.3/MIDA 4.3.csv") %>%
  filter(between(styear, 1945, 2010)) 

# One record per participant per dispute
mid_dyad.df <- import("./data/independent variables/MID 4.3/MIDB 4.3.csv") %>%
  filter(between(styear, 1945, 2010)) %>%
  mutate(cname = countrycode(stabb, "cowc", "cow.name", 
                             custom_match = c("GFR" = "German Federal Republic",
                                              "RUM" = "Romania",
                                              "USR" = "Russia",
                                              "VTM" = "Vietnam",
                                              "ZAI" = "Democratic Republic of the Congo")))

# Join conflict parties to incident level data
### ------------------------------------------------------------------------ ###
mid_dyad.df <- mid_dyad.df %>%
  group_by(dispnum3, sidea) %>%
  summarise(cntrya = paste(cname, collapse = ", "))

# Separate side A and B
sideA <- mid_dyad.df %>%
  filter(sidea == 1)

sideB <- mid_dyad.df %>%
  filter(sidea == 0) %>%
  select(-sidea) %>%
  rename("cntryb" = "cntrya")

# Merge again into separate columns (one row per conflict)
mid_dyad.df <- sideA %>%
  select(dispnum3, cntrya) %>%
  left_join(sideB)

# Checks
which(!(sideA$dispnum3 %in% sideB$dispnum3)) # col: 25, dispnum3: 258

# Join to incident data
mid_inc_dyad.df <- mid_inc.df %>%
  left_join(mid_dyad.df, by = c("dispnum3"))

# Join the data on the MID narratives (Gibler 2018)
### ------------------------------------------------------------------------ ###
# Load the data created in MID_Narratives_preprocessing.R
mid_narratives <- import("./data/independent variables/Gibler2018_MID_join.rds")

# Join to the dyadic MID data
## !!! Check whether all conflicts have a matching narrative
mid_inc_dyad.df <- mid_inc_dyad.df %>%
  left_join(mid_narratives, by = c("dispnum3" = "dispnum"))
