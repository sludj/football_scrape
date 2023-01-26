# libraries
library(tidyverse)
library(cluster)

# Combine the data
fantasycalc <- read_csv("final_data/fantasycalc_values.csv") %>% 
  mutate(fc_rank_overall = row_number())
ktc <- read_csv("final_data/ktc_values.csv") %>% 
  mutate(ktc_rank = row_number()) %>% 
  rename(ktc_value = player_values)
fc_map <- read_csv("mapping_files/fc_map.csv")

# map ktc names to fc
fc_mapped <- fantasycalc %>% 
  left_join(fc_map) %>% 
  mutate(combined_name = coalesce(ktc_name, player_name)) %>% 
  select(player_name = combined_name, age, fc_value = value, fc_rank = posRank, fc_rank_overall)

# Join together
combined <- ktc %>% 
  left_join(fc_mapped)

# Check what doesn't map (if needed) ---------
# no_join_fc <- anti_join(fantasycalc, ktc)
# no_join_ktc <- anti_join(ktc, fantasycalc)
# 
# no_join_fc_map_list <- no_join_fc %>% 
#   filter(!str_detect(player_name, "202*"))
# no_join_ktc_map_list <- no_join_ktc %>% 
#   filter(!str_detect(player_name, "202*"))

# If needed, we get this list to map against KTC
#write_csv(no_join_fc_map_list, "mapping_files/fc_map_list.csv")

# calculate difference in value and rank
combined_analysis <- combined %>% 
  mutate(rank_diff = ktc_rank - fc_rank_overall,
         value_avg = (ktc_value + fc_value) / 2) 

combined_analysis_drop_na <- combined_analysis %>% 
  drop_na(value_avg) %>% 
  arrange(desc(value_avg)) %>% 
  mutate(avg_value_rank = row_number())


top_50 <- combined_analysis %>% 
  filter(ktc_rank <= 50)
