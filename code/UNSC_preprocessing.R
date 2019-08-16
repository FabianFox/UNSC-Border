# UNSC resolutions - Preprocessing

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quanteda, tif)

# Read resolutions (data created in: UNSC_load_pdf.R)
### ------------------------------------------------------------------------ ###
res_files <- readRDS(file = "./output/UNSC_corpus.rds") %>%
  rename(text = token) # adhere to tif-standard

# Data Preparation
# Overview of necessary decisions: Denny & Sperling (2018: 170-172)
# Recommendations (preprocessing): Welbers et al. (2017: 250-252) 

# Basic analyis: distribution
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