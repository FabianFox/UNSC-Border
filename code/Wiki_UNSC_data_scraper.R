# UNSC resolutions - Information from Wikipedia

# Notes & Issues:
# - 

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, quanteda, tif, janitor, lubridate, rio)

# Create a list of links to map over
### ------------------------------------------------------------------------ ###
base_url <- "https://en.wikipedia.org/wiki/List_of_United_Nations_Security_Council_Resolutions_"

# Individual pages
wiki_res.df <- tibble(
  from = seq(1, 2410, 100),
  to = seq(100, 2500, 100)
) %>%
  mutate(pages = paste0(base_url, from, "_to_", to)) %>%
  select(-from, -to)

# Create a function that gathers the individual wikipedia tables across pages
### ------------------------------------------------------------------------ ###
wiki_scrape <- function(x){
  read_html(x) %>%
  html_table(fill = TRUE) %>%
  .[[3]]
}

# Safely
wiki_scrape <- possibly(wiki_scrape, NULL)

# Scrape and clean wiki tables
### ------------------------------------------------------------------------ ###
wiki_tables <- wiki_res.df %>%
  mutate(data = map(pages, 
                    ~wiki_scrape(.x))) %>%
  unnest(cols = data) %>%
  clean_names() %>%
  mutate(date = parse_date_time(date, "d!%b!%Y!"))

# Export
### ------------------------------------------------------------------------ ###
# export(wiki_tables, "./output/Wiki_meta_df.rds")