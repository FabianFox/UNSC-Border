# Preparing UNSC resolutions

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, pdftools, tesseract, readtext, tif)

# Get the location of the resolutions and import them; adhering to the tif
### ------------------------------------------------------------------------ ###
# standards for "tokens" (https://github.com/ropensci/tif)
res_files <- tibble(
  doc_id = list.files("./data/resolutions/", pattern = ".pdf"),
  doc_loc = paste0("./data/resolutions/", doc_id), 
  token = map(doc_loc, ~ pdf_text(.) %>%
                         unlist(.)) 
)

# Find resolutions without OCR
### ------------------------------------------------------------------------ ###
res_files_ocr <- res_files %>%
  mutate(doc_length = str_length(token),
         doc_ocr = ifelse(doc_length > 100, TRUE, FALSE)) %>%
  filter(doc_ocr == FALSE)

# Use tesseract::ocr on docs without ocr
# resolutions are saved in "./data/resolutions/png"
res_files_ocr <- res_files_ocr %>%
  mutate(token = ocr(doc_loc, engine = tesseract("eng")))

# Read the text from the docs that have OCR now
# resolutions with multiple pages are split
ocr_files <- tibble(
  doc_id = list.files("./data/resolutions/png", pattern = "*png"),
  token = ocr(doc_id, engine = tesseract("eng"))
)

# Join multipaged resolutions back together
ocr_join <- ocr_files %>%
  mutate(doc_id = unlist(str_extract_all(doc_id, ".+(?=_[:digit:]+.png)"))) %>%
  group_by(doc_id) %>%
  summarise(token = paste(token, collapse = " "))

# Saved as "./data/resolutions/png/res_files_png.rds"
ocr_join <- readRDS("./data/resolutions/png/res_files_png.rds")

# Join back to the original data
res_files <- res_files %>%
  mutate(doc_id = unlist(str_extract_all(doc_id, ".+(?=.pdf)")))

# Join back resolutions that now have OCR
res_files$token_ocr <- ocr_join[match(res_files$doc_id, ocr_join$doc_id),]$token 

# Combine tokens and tokens with newly added OCR
res_files <- res_files %>%
  mutate(doc_length = str_length(token),
         token = ifelse(doc_length < 100, token_ocr, token))

# Flatten list-column "token"
res_files <- res_files %>%
  group_by(doc_id) %>%
  summarise(token = paste(token, collapse = " "))

# Results saved as "./output/UNSC_corpus.rds"
# saveRDS(res_files, file = "./output/UNSC_corpus.rds")