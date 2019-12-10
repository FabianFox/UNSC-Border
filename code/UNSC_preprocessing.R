# UNSC resolutions - Preprocessing

# Notes & Issues:
# - add metadata from UNSC_scraper_voting.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quanteda, tif, spacyr, rio)

# Read resolutions (data created in: UNSC_load_pdf.R)
### ------------------------------------------------------------------------ ###
res_files <- import(file = "./output/UNSC_corpus.rds") %>%
  rename(text = token) %>% # adhere to tif-standard 
  mutate(resolution_num = strtoi(str_extract(doc_id, "(?<=S_RES_)[:digit:]+")))

# Resolution 1107 is mistakenly coded as resolution 11
res_files[which(duplicated(res_files$resolution_num) == TRUE),]$resolution_num <- 1107

# Meta-data
res_meta <- import("./output/UNSC_voting_data.rds") %>%
  mutate(resolution = map_chr(voting_df, "resolution"),
         resolution_num = strtoi(str_extract(resolution, "(?<=S/RES/)[:digit:]+")))

# Meta information from Wikipedia
res_wiki_meta <- import("./output/Wiki_meta_df.rds") %>%
  rename(resolution_num = resolution) %>%
  nest(data = c(pages, date, vote, concerns))

# Join meta information and resolution
unsc.df <- res_files %>%
  left_join(res_meta, by = "resolution_num") %>%
  left_join(res_wiki_meta, by = "resolution_num") %>%
  arrange(resolution_num)

# Sanity checks
### ------------------------------------------------------------------------ ###

# Equal number of resolution in the datasets
# Resolution 2481 & 2482 missing in res_meta
res_files[which(!res_files$resolution_num %in% res_meta$resolution_num), 
          c("doc_id", "resolution_num")]


# Data Preparation
# Overview of necessary decisions: Denny & Sperling (2018: 170-172)
# Recommendations (preprocessing): Welbers et al. (2017: 250-252) 

# Preprocessing
### ------------------------------------------------------------------------ ###
# Basic preprocessing
res_files <- res_files %>%
  mutate(border = str_detect(text, "border"),
         year = strtoi(stringi::stri_extract_last_regex(doc_id, "[:digit:]{4}")),
         text = str_replace_all(text, "(\\\\r?\\\\n)|(\\\\n)", " "),               # replace carriage return (\r) and line feed (\n)
         text = str_replace_all(text, "\\s+", " "))                                # delete excessive spaces

# Filter to border disputes:
#  filter(border == TRUE) %>%
#  select(-border) 

# Transform into a quanteda corpus
res_qcorpus <- corpus(res_files,
                     docid_field = "doc_id",
                     text_field = "text")

# Transfrom to quanteda document-feature matrix
res_dfm <- dfm(res_qcorpus, tolower = TRUE, stem = TRUE, 
               remove_punct = TRUE, remove = stopwords("english"),
               remove_numbers = TRUE)