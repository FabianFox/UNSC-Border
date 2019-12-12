# Read MID Narratives from Gibler (2018) International Conflicts, 1816–2010. MID Narratives.

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, pdftools, tidytext, rio)

# Directory
location <- "Y:/Grenzen der Welt/Literatur/Gibler 2018 - International Conflicts, 1816-2010_ Militarized Interstate Dispute Narratives.pdf"
  
# (1) Read in the pdf-file 
# gibler.txt <- pdf_text(location)
# export(gibler.txt, "./data/independent variables/Gibler2018_MID.rds")
gibler.txt <- import("./data/independent variables/Gibler2018_MID.rds")

# (2) Collapse and turn into tibble
gibler.pst <- paste0(gibler.txt, collapse = " ") %>%
  enframe()

# (3) tidytext-format and basic cleaning
gibler.tidy <- gibler.pst %>%
  mutate(value = str_replace_all(value, "[:punct:]", ""),
         value = str_replace_all(value, "Dispute Number", "dispute_number")) %>%
  unnest_tokens(word, value)

# (4) Cut the corpus into individual narratives
gibler.tidy <- gibler.tidy %>%
  mutate(mid_id = str_detect(word, "dispute_number")) %>%
  group_by(mid_id == TRUE) %>%
  mutate(row_id = ifelse(mid_id == TRUE, 1:n(), NA)) %>%
  ungroup() %>%
  fill(row_id, .direction = "down") %>%
  filter(!is.na(row_id)) %>%
  group_by(row_id) %>%
  summarise(meta = paste0(word, collapse = " "))

# (5) Clean the narratives
gibler.tidy <- gibler.tidy %>%
  mutate(dispnum = strtoi(str_extract(meta, "(?<=dispute_number )[:digit:]+")),
         narrative = str_extract_all(meta, "(?<=narrative ).+")) %>%
  select(dispnum, narrative)

# Export
export(gibler.tidy, "./data/independent variables/Gibler2018_MID_join.rds")
