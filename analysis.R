library(readxl)
library(tidyverse)

var_names <- c(
  "record_id",
  "First Name",
  "Middle Name",
  "Last Name",
  "Participants",
  "Cohort",
  "stage",
  "com_discipline",
  "region",
  "Role",
  "Title",
  "Abstract",
  "Award Type",
  "Amount",
  "Start Date",
  "End Date"
)

data_nsf <- read_excel("NSF_demographic.xlsx", sheet = 1)[,var_names]
data_nih <- read_excel("NIH_demographic.xlsx", sheet = 1)[,var_names]

data_nsf$agency <- "NSF"
data_nih$agency <- "NIH"

data_merged <- rbind(data_nih, data_nsf) %>%
  filter(!is.na(Cohort) & !is.na(Amount)) %>% 
  tidyr::unite(
    name,
    `First Name`, `Middle Name`, `Last Name`,
    sep = " ",
    na.rm = TRUE
  )
  
openxlsx::write.xlsx(
  x = as.data.frame(data_merged),
  file = "data_merged_clean.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)
################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

data_desc <- data_merged %>%
  mutate(
    name = trimws(name),
    Cohort = factor(Cohort),
    stage = na_if(trimws(stage), ""),
    com_discipline = na_if(trimws(com_discipline), ""),
    region = na_if(trimws(region), ""),
    Role = na_if(trimws(Role), ""),
    agency = na_if(trimws(agency), ""),
    `Award Type` = na_if(trimws(`Award Type`), ""),
    Amount = as.numeric(Amount)
  )

#--- overall summary
overall_summary <- data_desc %>%
  summarise(
    n_records = n(),
    n_people = n_distinct(name, na.rm = TRUE),
    n_cohorts = n_distinct(Cohort, na.rm = TRUE),
    n_award_types = n_distinct(`Award Type`, na.rm = TRUE),
    total_amount = sum(Amount, na.rm = TRUE)
  )

overall_summary

#--- by cohort
data_desc %>% 
  group_by(Cohort) %>% 
  reframe(
    n_people = length(unique(name)),
    n_award = n(),
    amount = sum(Amount, na.rm = TRUE)
    )
#--- growth
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(forcats)

awards <- data_merged %>%
  mutate(
    cohort_year = readr::parse_number(as.character(Cohort)),
    start_date = as.Date(
      parse_date_time(
        `Start Date`,
        orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS")
      )
    ),
    award_year = year(start_date),
    relative_year = award_year - cohort_year,
    amount = as.numeric(Amount),
    role_clean = case_when(
      tolower(trimws(Role)) == "pi" ~ "PI",
      tolower(trimws(Role)) == "co-pi" ~ "Co-PI",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(name),
    !is.na(cohort_year),
    !is.na(award_year)
  )

activity_by_year <- awards %>%
  group_by(relative_year) %>%
  summarise(
    n_researchers = n_distinct(name),
    n_awards = n(),
    n_PI_awards = sum(role_clean == "PI", na.rm = TRUE),
    n_CoPI_awards = sum(role_clean == "Co-PI", na.rm = TRUE),
    PI_share = ifelse(
      sum(!is.na(role_clean)) > 0,
      mean(role_clean == "PI", na.rm = TRUE),
      NA_real_
    ),
    total_recorded_amount = sum(amount, na.rm = TRUE),
    median_award_amount = ifelse(
      all(is.na(amount)), NA_real_,
      median(amount, na.rm = TRUE)
    ),
    n_agencies = n_distinct(agency, na.rm = TRUE),
    n_disciplines = n_distinct(com_discipline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(relative_year)

activity_by_year


activity_by_year %>% 
  filter(relative_year < 5) %>% 
  ggplot(aes(x = relative_year, y = n_awards)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(
    min(activity_by_year$relative_year),
    4, by = 1
  )) +
  coord_cartesian(xlim = c(
    min(activity_by_year$relative_year),
    4)) +
  labs(
    title = "Award Activity Before and After Program Participation",
    subtitle = "Award records grouped by year of award initiation",
    x = "Years relative to program participation",
    y = "Number of award records",
    color = "Cohort"
  ) +
  theme_minimal()



















