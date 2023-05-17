library(tidyverse)

fantasy_data <- read_csv("final_data/pfr_fantasy_2010_2022.csv")

# Qb analysis
qbs <- fantasy_data %>% 
  filter(FantPos == "QB")

qbs_ppg <- qbs %>% 
  mutate(fppg = `Fantasy FantPt` / `Games GS`)

qbs_ppg_sort <- qbs_ppg %>% 
  filter(`Games GS` >= 8) %>% 
  group_by(year) %>% 
  arrange(year, desc(fppg)) %>% 
  mutate(fppg_rank = rank(-fppg)) 

qbs_ppg_gap <- qbs_ppg_sort %>% 
  filter(fppg_rank == 8 | fppg_rank == 25)

# RB analysis for kyles boys
rbs_to_check <- c("Dalvin Cook", "James Conner", "David Montgomery")
rbs <- fantasy_data %>% 
  filter(FantPos == "RB",
         Player %in% rbs_to_check) %>% 
  mutate(rush_att_and_rec_p_game = (`Rushing Att` + `Receiving Rec`) / `Games G`) %>% 
  select(Player, year, rush_att_and_rec_p_game)
