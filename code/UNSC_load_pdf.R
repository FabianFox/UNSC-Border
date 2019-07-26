# Preparing UNSC resolutions

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, pdftools)

# Get the location of the resolutions
res_files <- tibble(
  resolution_id = dir("./data/resolutions"),
  location = paste0("./data/resolutions/", resolution_id), 
  resolution_txt = paste(map(location, ~ (pdf_text(.))), collapse = " ")
)

