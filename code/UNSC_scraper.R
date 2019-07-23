# Downloading UNSC resolutions

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest)

# TO DO:
# pdfs are embedded in an iframe but the URL can still be manipulated in order to
# download the files
# i.e. https://undocs.org/pdf?symbol=en/S/RES/572(1985)

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
  unnest(resolution_links, .preserve = resolution_table) %>%
  mutate(resolution_id = flatten_chr(str_extract_all(resolution_links, "(?<=org/).+"))) %>%
  distinct(resolution_links, .keep_all = TRUE) %>%          # some links are included multiple times
  filter(str_detect(resolution_links, "RES") == TRUE) %>%   # some letters that are not resolutions are included
  mutate(pdf_link = paste0("https://undocs.org/pdf?symbol=en/", resolution_id)) # url that leads directly to the pdf (no iframe)

# (2) Checks
### ------------------------------------------------------------------------ ###
# Check whether all resolutions have been detected
all_resolutions <- links.df %>%
  distinct(resolution_links, .keep_all = TRUE) %>%
  filter(str_detect(resolution_links, "RES") == TRUE) %>%
  mutate(cons_num = strtoi(str_extract_all(resolution_id, "(?<=/)[:digit:]+"))) %>% #|%|/]
  arrange(cons_num) %>%
  mutate(cons_check = c(FALSE, diff(cons_num) != 1))

# Not consecutive are 12 572 1106
which(all_resolutions$cons_check == TRUE) 

# Treatment:
# Resolution 571 is not linked on the website but available: https://undocs.org/S/RES/571(1985)
# Resolution 1107 is misformed but included as https://undocs.org/S/RES/11%2007(1997)
# Add RES 571 manually:
links.df <- bind_rows(links.df, 
                    tibble(year = "1985", 
                    links = "https://www.un.org/securitycouncil/content/resolutions-adopted-security-council-1985", 
                    resolution_table = links.df$resolution_table[1922],    # Same as the others on the page for 1985 resolutions
                    resolution_links = "https://undocs.org/S/RES/571(1985)",
                    resolution_id = "S/RES/571(1985)",
                    pdf_link = "https://undocs.org/pdf?symbol=en/S/RES/571(1985)"))

# (3) Download the resolutions
### ------------------------------------------------------------------------ ###
# Data available as .pdf and .docx

# Add column with naming convention
links.df <- links.df %>%
  mutate(filename = str_replace_all(resolution_id, "[:punct:]", "_"))

map2(
  .x = links.df$pdf_link[1:10],
  .y = links.df$filename[1:10],
  .f = ~ {
    Sys.sleep(sample(seq(2, 10, 0.5), 1))
    download.file(url = .x, destfile = paste0("./data/resolutions/", .y, ".pdf"), method = "libcurl", mode = "wb")
  }
)
