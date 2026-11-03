library(readxl)
library(tidyverse)

data_vars <- setdiff(
  colnames(read.csv("mhti_applicants.csv")),
  c("first_name", "last_name")
  )

var_names <- c(
  "Full Name",
  "Participants",
  data_vars,
  "Role",
  "Title",
  "Abstract",
  "Award Type",
  "Amount",
  "Start Date",
  "End Date"
)

data_nsf <- read_excel("NSF_demographic3.xlsx", sheet = 1)[,var_names]
data_nih <- read_excel("NIH_demographic3.xlsx", sheet = 1)[,var_names]

data_nsf$agency <- "NSF"
data_nih$agency <- "NIH"

data_merged <- rbind(data_nih, data_nsf) %>%
  filter(!is.na(Amount)) %>% 
  rename("name" = "Full Name")

openxlsx::write.xlsx(
  x = as.data.frame(data_merged),
  file = "data_merged_clean3.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)
################################################################################
