# Downloading UNSC resolutions

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest)

# (1) Locate the resolutions
### ------------------------------------------------------------------------ ###

# Root URL
rootURL <- "https://www.un.org/securitycouncil/content/resolutions"

# Get the links leading to the yearly resolutions and create a tbl
links <- read_html(rootURL) %>%
  html_nodes(".even a") %>%
  html_attr(., "href") %>%
  tibble(
    year = str_extract(., "(?<=-)[:digit:]{4}"),    # Some href contain full links
    links = ifelse(str_detect(., "https") == FALSE, # hence the ifelse-condition
      paste0("https://www.un.org", .), .
    )
  ) %>%
  select(-1)

# Get the links to the individual resolution and the accompanying information
links.df <- links %>%
  mutate(
    resolution_links = map(   # Retain the links to the
      links,                  # individual resolutions
      ~ read_html(.) %>%
        html_nodes("td a") %>%
        html_attr(., "href")
    ))

# Function that extracts the tables
scrape_table <- function(x){
  
  x %>%
    read_html() %>%
    html_nodes(".table") %>%    # table?
    html_table() %>%
    flatten_df()
  
}

# Scrape safely
scrape_table_possibly <- possibly(scrape_table, otherwise = NULL)
    
# Get the information from the tables
links.df <- links.df %>%
  mutate(resolution_table = map(links, ~scrape_table_possibly(.x))
)

# Rename the listed dfs
links.df <- links.df %>%
  mutate(
    tbl_length = map(resolution_table, ncol),
    names = case_when(
      tbl_length == 1 ~ list(c("topic")),
      tbl_length == 2 ~ list(c("resolution", "topic")),
      tbl_length == 3 ~ list(c("resolution", "date", "topic")),
      TRUE ~ list(NULL)
    ),
    resolution_table = map2(.x = resolution_table, .y = names, ~ set_names(.x, .y))
  ) %>%
  select(-c(tbl_length, names)) %>%
  unnest(resolution_table, .preserve = resolution_links)
  mutate(resolution_no = str_extract_all(resolution_links, "(?<=org/).+"))

# (2) Download the resolutions
### ------------------------------------------------------------------------ ###
# Data available as .pdf and .docx

### ADJUST

map2(
  .x = cquery.df$csv.link,
  .y = cquery.df$iso2,
  .f = ~ {
    Sys.sleep(sample(seq(2, 10, 0.5), 1))
    download.file(url = .x, destfile = paste0("./GIZ-files/", .y, ".csv"), method = "curl")
  }
)

###

# Check the length whether the length of the respective vectors is constant
# across approaches (currently it isn't)
tbl_rsl <- unnest(links.df, resolution_table)
link_rsl <- unnest(links.df, resolution_links)
