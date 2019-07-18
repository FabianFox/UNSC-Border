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
links.df <- links[c(1:3, 54),] %>%
  mutate(
    resolution_links = map(   # Retain the links to the
      links,                  # individual resolutions
      ~ read_html(.) %>%
        html_nodes("td a") %>%
        html_attr(., "href")
    ),
    resolution_table = map(
      links,
      ~ read_html(.) %>%
        html_table() %>%
        .[[1]]
    )
  ) 
# %>%
#  unnest(resolution_links) %>%
#  mutate(resolution = str_extract_all(resolution_links, "(?<=org/).+"))

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
