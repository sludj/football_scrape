# Combine the PFR fantasy and draft to get info for a regression
# to predict fantasy performance

# Setup & Import
library(tidyverse)
library(janitor)

fantasy_data <- read_csv("final_data/pfr_fantasy_2010_2022.csv") %>% 
  clean_names() %>% 
  rename(pos = fant_pos)
draft_data <- read_csv("final_data/pfr_draft_2010_2022.csv") %>% 
  clean_names() %>% 
  rename(draft_age = age, draft_year = year) %>% 
  select(-to)

glimpse(fantasy_data)
glimpse(draft_data)

combined_fantasy_draft <- left_join(fantasy_data, draft_data,
                                    by = c("player", "pos"))
