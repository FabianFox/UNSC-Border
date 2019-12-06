# UNSC resolutions - Information from Wikipedia

# Notes & Issues:
# - add metadata from UNSC_scraper_voting.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, quanteda, tif)

# Read resolutions
### ------------------------------------------------------------------------ ###

base_url <- "https://en.wikipedia.org/wiki/List_of_United_Nations_Security_Council_Resolutions_"

wiki_res.df <- tibble(
  from = seq(1, 2410, 100),
  to = seq(100, 2500, 100)
) %>%
  mutate(pages = paste0(base_url, from, "_to_", to)) %>%
  select(-from, -to)

# Create a function that gathers the individual wikipedia tables across pages
wiki_scrape <- function(x){
  read_html(x) %>%
  html_table(fill = TRUE) %>%
  .[[3]]
}

# Safely
wiki_scrape <- possibly(wiki_scrape, NULL)

# Scrape
wiki_tables <- wiki_res.df %>%
  mutate(data = map(pages, 
                    ~wiki_scrape(.x))) 

# TO DO
# Unnest and clean (date etc.)
# Join to UNSC_meta