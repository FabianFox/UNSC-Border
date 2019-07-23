# Preparing UNSC resolutions

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, pdftools)

# Get the location of the resolutions
res_files <- tibble(
  resolution_id = dir("./data"),
  location = paste0("./data/", dir("./data")),
  resolution_txt = paste(map(location, ~ (pdf_text(.))), collapse = " ")
)
