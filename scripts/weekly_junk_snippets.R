################################################################################
#
# Weekly Junk Analysis Program
#
# Purpose: Take exported rankings and generate a value for each team
# then output snippets of analysis for inclusion in the weekly junk
#
# Dependency: For most up to date data, run these programs first
#        1. scripts/fantasycalc_scrape.R
#        2. scripts/ktc_scrape.R
#        3. scripts/combine_data_analysis.R
#
# Imports: final_data/combined_analysis_weighted_bi_norm_values.csv
#
# Steps: 1. Setup & Import
#        2. Generate team values
#        3. Generate weekly junk snippets
#        4. Save image & export archive
# 
################################################################################

# Step 1: Setup & Import -------------------------------------------------------

# Libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)

# Import files
full_data <- read_csv("final_data/combined_analysis_weighted_bi_norm_values.csv")
prev_full_data <- read_csv("final_data/z_archive/full_data_wj.csv")
prev_total_by_team <- read_csv("final_data/z_archive/total_by_team_wj.csv")

# Step 2: Generate team values -------------------------------------------------

total_by_team <- full_data %>% 
  group_by(team_name, fc_position) %>% 
  summarize(total_value_by_position = sum(weighted_avg_norm)) %>% 
  drop_na(team_name)

# Step 3: Generate weekly junk snippets ----------------------------------------
ggplot(data = total_by_team, mapping = aes(x = reorder(team_name, -total_value_by_position, sum), 
                                           y = total_value_by_position,
                                           fill = fc_position)) + 
  geom_col(position = "stack") +
  theme_tufte() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "", y = "Community Consensus Weighted Value",
       title = "Community Consensus Team Values",
       fill = "Position")

# Change in team value
value_change <- full_data %>% 
  left_join(select(prev_full_data, player_name, prev_value = weighted_avg_norm), by = "player_name") %>% 
  mutate(change_in_value = weighted_avg_norm - prev_value,
         perc_change_in_value = change_in_value / prev_value * 100)

# Step 4: Save image & export archive ----------------------------------------
ggsave("final_data/weekly_junk/cctv.pdf")
write_csv(full_data, "final_data/z_archive/full_data_wj.csv")
write_csv(total_by_team, "final_data/z_archive/total_by_team_wj.csv")
