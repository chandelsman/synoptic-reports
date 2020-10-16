# This script selects a random 10% of all cases for each site/procedure

# load libraries
library(tidyverse)


# load data
breast <- readxl::read_excel("./data/synoptic.xlsx", sheet = 2)
colorectal <- readxl::read_excel("./data/synoptic.xlsx", sheet = 3)
kidney <- readxl::read_excel("./data/synoptic.xlsx", sheet = 4)
prostate <- readxl::read_excel("./data/synoptic.xlsx", sheet = 5)
testes <- readxl::read_excel("./data/synoptic.xlsx", sheet = 6)
bladder <- readxl::read_excel("./data/synoptic.xlsx", sheet = 7)
hysterectomy <- readxl::read_excel("./data/synoptic.xlsx", sheet = 8)
lung <- readxl::read_excel("./data/synoptic.xlsx", sheet = 9)
pancreas <- readxl::read_excel("./data/synoptic.xlsx", sheet = 10)


# make site variable for grouping
breast <- breast %>% 
 mutate(site = "Breast")
colorectal <- colorectal %>% 
  mutate(site = "Colorectal")
kidney <- kidney %>% 
  mutate(site = "Kidney")
prostate <- prostate %>% 
  mutate(site = "Prostate")
testes <- testes %>% 
  mutate(site = "Testes")
bladder <- bladder %>% 
  mutate(site = "Bladder")
hysterectomy <- hysterectomy %>% 
  mutate(site = "Hysterectomy")
lung <- lung %>% 
  mutate(site = "Lung")
pancreas <- pancreas %>% 
  mutate(site = "Pancreas")
  

# combine data sets
synoptic_candidates <- 
  bind_rows(
    breast, colorectal, kidney, 
    prostate, testes, bladder, 
    hysterectomy, lung, pancreas
  )


# make sampling function to include at least one accession when n < 10
sample_up <- function(.data, frac) {
  sample_n(.data, ceiling({{frac}} * n()) )
}

# select 10% of cases for each "site"
synoptic_review <- synoptic_candidates %>% 
  group_by(site) %>% 
  sample_up(0.1) %>% 
  ungroup() %>% 
  rename(Site = site) %>% 
  select(c(Site, Accession))


# make Excel file with list of sampled site/procedures and accessions
writexl::write_xlsx(synoptic_review, "./output/synoptic_review_2020-05-12.xlsx")
  