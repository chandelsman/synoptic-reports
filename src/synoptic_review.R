# This script selects a random 10% of all cases for each site/procedure

# load libraries
library(dplyr)
library(here)
library(lubridate)

# load "sample_up" function to select random cases and round up if n <10
source(here("src", "fcn_random.R"))

# load synoptic data
synoptics_raw <-
  list.files(path = here("data"),
             pattern = "(\\d){4}(\\D\\d-).+\\.xls",
             full.names = TRUE) %>%
  sapply(readxl::read_excel, simplify = FALSE) %>%
  bind_rows() %>% 
  mutate(created_date = date(mdy_hm(created_date))) %>% 
  filter(quarter(created_date) == quarter(Sys.Date()) - 1)

# filter, categorize data and select relevant cases
synoptics <- 
  synoptics_raw %>% 
  filter(
    client != "UCHEALTH SOUTH",
    `result type` == "Surgical",
    !grepl("skin", `site label`, ignore.case = TRUE),
    !grepl("biopsy", `site label`, ignore.case = TRUE),
    !grepl("biopsies", `site label`, ignore.case = TRUE),
    !grepl("excision", `site label`, ignore.case = TRUE),
    !grepl("blood", `site label`, ignore.case = TRUE),
    !grepl("curettage", `site label`, ignore.case = TRUE),
    !grepl("leep", `site label`, ignore.case = TRUE),
    !grepl("trans-urethral", `site label`, ignore.case = TRUE),
    !grepl("transurethral", `site label`, ignore.case = TRUE),
    !grepl("turbt", `site label`, ignore.case = TRUE),
    !grepl("polyp", `site label`, ignore.case = TRUE),
    !grepl("gallbladder", `site label`, ignore.case = TRUE)
  ) %>% 
  mutate(
    Site = 
      case_when(
        grepl("colon", `site label`, ignore.case = TRUE) ~ "Colon",
        grepl("breast", `site label`, ignore.case = TRUE) ~ "Breast",
        grepl("prostate", `site label`, ignore.case = TRUE) ~ "Prostate",
        grepl("uterus", `site label`, ignore.case = TRUE) | 
          grepl("omentum", `site label`, ignore.case = TRUE) ~ "Uterus",
        grepl("lung", `site label`, ignore.case = TRUE) ~ "lung",
        grepl("kidney", `site label`, ignore.case = TRUE) ~ "Kidney",
        grepl("pancreas", `site label`, ignore.case = TRUE) | 
          grepl("whipple", `site label`, ignore.case = TRUE) ~ "Pancreas",
        grepl("testis", `site label`, ignore.case = TRUE) ~ "Testis",
        grepl("bladder", `site label`, ignore.case = TRUE) | 
          grepl("cystectomy", `site label`, ignore.case = TRUE) | 
          grepl("cystoprostatectomy", `site label`, ignore.case = TRUE) ~ "Bladder",
        grepl("tonsil", `site label`, ignore.case = TRUE) |
          grepl("adenoid", `site label`, ignore.case = TRUE) ~ "Tonsils / Adenoids",
        TRUE ~ "---"
      )
  ) %>% 
  filter(Site != "---") %>% 
  group_by(Site) %>% 
  sample_up(0.15) %>% 
  select(c(created_date, Site, Accession))

# make Excel file with list of sampled sites and corresponding accessions
writexl::write_xlsx(synoptics, here("output", "2021_Q1_synoptics.xlsx"))
