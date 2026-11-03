library(readxl)
library(tidyverse)

var_names <- c(
  "Full Name",
  "Participants",
  "cohort",
  "affiliation",
  "openalex_author_id",
  "google_scholar_url",
  "google_scholar_user_id",
  "note",
  "Role",
  "Title",
  "Abstract",
  "Award Type",
  "Amount",
  "Start Date",
  "End Date"
)

data_nsf <- read_excel("NSF_demographic2.xlsx", sheet = 1)[,var_names]
data_nih <- read_excel("NIH_demographic2.xlsx", sheet = 1)[,var_names]

data_nsf$agency <- "NSF"
data_nih$agency <- "NIH"

data_merged <- rbind(data_nih, data_nsf) %>%
  filter(!is.na(cohort) & !is.na(Amount)) %>% 
  rename("Cohort" = "cohort",
         "name" = "Full Name")

openxlsx::write.xlsx(
  x = as.data.frame(data_merged),
  file = "data_merged_clean2.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)
################################################################################
