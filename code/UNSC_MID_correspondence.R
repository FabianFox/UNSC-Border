# UNSC resolutions - Correspondence between resolutions & MID

# Notes & Issues:
# - add metadata from UNSC_scraper_voting.R

# Tasks:
# 1. Extraction country names from strings
# 2. Add metadata: 

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, spacyr)

# Named entity recognition
### ------------------------------------------------------------------------ ###
# Initialize spacy
spacy_initialize("en_core_web_sm")

# Smaller random sample of articles
set.seed(4535)
floor(runif(3, min=0, max=101))
spacy_test <- res_files %>%
  slice(1:20)

# Named entity recognition
spacy_prs <- spacy_parse(spacy_test, tag = TRUE)
spacy_ext <- entity_extract(spacy_prs)

# filter to recognized countries/locations
cntry <- spacy_prs %>%
  filter(entity == "GPE_B" | entity == "GPE_I")



# TF-IDF
### ------------------------------------------------------------------------ ###
res_tf <- dfm_tfidf(res_dfm, base = 10)
topfeatures(res_tf[15, ], n = 20)

# Topic modeling
### ------------------------------------------------------------------------ ###
t_model <- stm::stm(res_dfm, K = 10)