# UNSC resolutions - Preprocessing

# Notes & Issues:
# - add metadata from UNSC_scraper_voting.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quanteda, tif)

# Read resolutions (data created in: UNSC_load_pdf.R)
### ------------------------------------------------------------------------ ###
res_files <- readRDS(file = "./output/UNSC_corpus.rds") %>%
  rename(text = token) # adhere to tif-standard

# Meta-data
res_meta <- readRDS("./output/UNSC_voting_data.rds")

# Data Preparation
# Overview of necessary decisions: Denny & Sperling (2018: 170-172)
# Recommendations (preprocessing): Welbers et al. (2017: 250-252) 

# Preprocessing
### ------------------------------------------------------------------------ ###
# Filter to resolutions containing "border"
res_files <- res_files %>%
  mutate(border = str_detect(text, "border")) %>%
  filter(border == TRUE) %>%
  select(-border)

# Transform into a quanteda corpus
res_qcorpus <- corpus(res_files,
                     docid_field = "doc_id",
                     text_field = "text")

# Transfrom to quanteda document-feature matrix
res_dfm <- dfm(res_qcorpus, tolower = TRUE, stem = TRUE, 
               remove_punct = TRUE, remove = stopwords("english"))
