# Combine the PFR fantasy and draft to get info for a regression
# to predict fantasy performance

# Setup & Import
library(tidyverse)
library(janitor)

fantasy_data <- read_csv("final_data/pfr_fantasy_2000_2022.csv")
draft_data <- read_csv("final_data/pfr_draft_2000_2022.csv")

glimpse(fantasy_data)
glimpse(draft_data)

fantasy_clean_names <- fantasy_data %>% 
  clean_names()

glimpse(fantasy_clean_names)

combined_fantasy_draft <- left_join(fantasy_clean_names, draft_data)
