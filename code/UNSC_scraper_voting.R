# Downloading UNSC resolutions voting data

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, janitor)

# Base df for iteration
unsc_voting <- tibble(
  url = paste0(
    "https://digitallibrary.un.org/search?ln=en&rg=100&jrec=",
    seq(1, 2480, 100),
    "&fct__2=Security+Council&fct__2=Security+Council&cc=Voting+Data&sf=year"
  )
)

# Locate links to voting tables
# Note: Apparently, there is no easy way to get the href-attribute
extract_vote <- function(x) {
  x %>%
    read_html() %>%
    html_nodes(".pagebodystripemiddle a") %>%
    html_attr("href") %>%
    .[str_detect(., "record") & !is.na(.)] # filter relevant links
}

# Safely
safely_extract_vote <- safely(extract_vote, otherwise = NULL)

# Get the links to the tables
unsc_voting <- unsc_voting %>%
  mutate(voting_table = map(
    url, ~ safely_extract_vote(.x)
  ))

# Clean URLs and unnest
unsc_voting <- unsc_voting %>%
  mutate(voting_table = map(voting_table, 1)) %>%
  unnest() %>%
  mutate(voting_table = paste0("https://digitallibrary.un.org", voting_table))

# Extract information from tables

# Function
table_fun <- function(x) {
  table <- read_html(x)

  col1 <- table %>%
    html_nodes("div.metadata-row > span:nth-child(1)") %>%
    html_text()

  col2 <- table %>%
    html_nodes("div.metadata-row > span:nth-child(2)") %>%
    html_text()

  df <- tibble(
    variable = col1,
    value = col2
  )
}

# Safely
safely_table_fun <- safely(table_fun, otherwise = NULL)

# Apply function
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(voting_table, ~ table_fun(.x)))


##############################################################################

# saveRDS(object = unsc_voting, file = "./output/UNSC_voting_data_raw.rds")
unsc_voting <- readRDS(file = "./output/UNSC_voting_data_raw.rds")

# Data cleaning
# Lift resolution-id from listed df to top level
unsc_voting <- unsc_voting %>%
  mutate(
    id = pluck(voting_df, "value"),
    id = pluck(id, 3)
  )

# Clean the listed table of information on the resolutions
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(
    voting_df,
    ~ spread(., key = variable, value = value) %>%
      clean_names() %>%
      mutate(., vote = paste0(" ", vote))
  ))

# Disentangle country/vote
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(
    voting_df,
    ~ mutate(.,
      country = str_split(vote, "(\\sY\\s|\\sN\\s|\\sA\\s)"),
      vote_split = str_extract_all(vote, "(\\sY\\s|\\sN\\s|\\sA\\s)")
    )
  ))

# Rewrite as map
for (i in seq_along(unsc_voting$voting_df)) {
  unsc_voting$voting_df[[i]]$country[[1]] <- unsc_voting$voting_df[[i]]$country[[1]][-1]
  unsc_voting$voting_df[[i]]$vote_split[[1]] <- paste(unsc_voting$voting_df[[i]]$vote_split[[1]], " ")
}

# Check particular values where number of votes != number of countries
unsc_voting <- unsc_voting %>%
  mutate(
    length_country =
      map_int(unsc_voting$voting_df, ~ lengths(.x$country)),
    length_vote_split =
      map_int(unsc_voting$voting_df, ~ lengths(.x$vote_split))
  ) %>%
  mutate(res_voted_on = length_country == length_vote_split) %>%
  select(-length_country, -length_vote_split)

# countries with comma in their name cause troubles, i.e. "GERMANY, FEDERAL REPUBLIC OF"
# str_replace them:
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(voting_df, ~.x %>%
    mutate(
      country = str_replace_all(
        country, "GERMANY, FEDERAL REPUBLIC OF", "FEDERAL REPUBLIC OF GERMANY"
      )
    )))

# separate member's vote and country
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(
    voting_df,
    ~ separate_rows(., country, vote_split, sep = ",")
  ))

# Clean the strings
unsc_voting <- unsc_voting %>%
  mutate(voting_df = map(
    voting_df,
    ~ mutate(.,
      country = str_trim(str_remove_all(country, "([:lower:]|[:punct:])"), "left"),
      vote = str_trim(str_remove_all(vote_split, "([:lower:]|[:punct:])"), "both")
    )
  ))
