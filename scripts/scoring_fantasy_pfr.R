# Calculate fantasy values for weird league scoring
# 
# Goal is to define function that lets us calculate different
# scoring types to see impact on ranks

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(janitor)

# Step 2: Import ---------------------------------------------------------------
pfr_fantasy <- read_csv("final_data/pfr_fantasy_2010_2023.csv") %>% 
  clean_names()

# see all of our variables to work with
names(pfr_fantasy)

# Step 3: Define function ------------------------------------------------------

custom_values <- pfr_fantasy %>% 
  filter(year == 2023) %>% 
  mutate(fantasy_score_custom = 
           # passing section
           (passing_yds * .04) +
           (passing_td * 6) +
           (passing_int * -2) +
           # Rushing section
           (rushing_yds * 0.15) +
           (rushing_td * 6) +
           (rushing_att * 0.1) +
           # Receiving section
           (ifelse(fant_pos == "TE", receiving_rec * 1.75, receiving_rec * 1)) +
           (receiving_yds * 0.1) +
           (receiving_td * 6),
         fantasy_score_adoc =
           # passing section
           (passing_yds * .04) +
           (passing_td * 4) +
           (passing_int * -2) +
           # Rushing section
           (rushing_yds * 0.1) +
           (rushing_td * 6) +
           # Receiving section
           (receiving_rec * .5) +
           (receiving_yds * 0.1) +
           (receiving_td * 6),
         custom_per_game = fantasy_score_custom / games_g,
         adoc_per_game = fantasy_score_adoc / games_g) %>% 
  arrange(desc(fantasy_score_custom)) %>% 
  mutate(custom_rank = row_number()) %>% 
  arrange(desc(fantasy_score_adoc)) %>% 
  mutate(adoc_rank = row_number(),
         change_in_rank = adoc_rank - custom_rank) %>% 
  select(player, age, fant_pos, fantasy_score_custom, custom_per_game, 
         fantasy_score_adoc, adoc_per_game,
         adoc_rank, custom_rank, change_in_rank)

write_csv(custom_values, "final_data/kyle_dynasty_points.csv")
  
