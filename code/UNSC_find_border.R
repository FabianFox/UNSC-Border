# Intermediate step: Find documents entailing the wordstem "border" and copy them
# to ./data/resolutions/border for detailed preprocessing
### ------------------------------------------------------------------------ ###

# Load/install packages / source
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(fs)

# Source 
source("./code/UNSC_merge.R")

# Preprocessing
### ------------------------------------------------------------------------ ###
# Overview of necessary decisions: Denny & Sperling (2018: 170-172)
# Recommendations (preprocessing): Welbers et al. (2017: 250-252) 

# Basic preprocessing
unsc.df <- unsc.df %>%
  mutate(year = strtoi(stringi::stri_extract_last_regex(doc_id, "[:digit:]{4}")),
         text = str_replace_all(text, "(\\\\r?\\\\n)|(\\\\n)", " "),               # replace carriage return (\r) and line feed (\n)
         text = str_replace_all(text, "\\s+", " "),                                # delete excessive whitespaces (between words)
         text = str_trim(text, "both"),                                            # trim leading/trailing whitespace
         text = str_to_lower(text),                                                # to lowercase
         border = str_detect(text, "border")) 

# Filter to border disputes issues:
unsc.border.df <- unsc.df %>%
  filter(border == TRUE) %>%
  select(-border) 

# Copy resolutions containing "border" to "./data/resolutions/border"
files_to_copy <- paste0("./data/resolutions/", unsc.border.df$doc_id, ".pdf")
new_folder <- paste0("./data/resolutions/border/", unsc.border.df$doc_id, ".pdf")

# Check whether files exist
all(file_exists(files_to_copy)) # TRUE

# Copy
file_copy(files_to_copy, new_folder)
