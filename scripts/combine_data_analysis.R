# libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(janitor)

options(scipen = 999)

# Combine the data
fantasycalc <- read_csv("final_data/fantasycalc_values.csv") %>% 
  mutate(fc_rank_overall = row_number(),
         fc_position = str_sub(posRank, end = 2))

fantasycalc <- read_delim("raw_data/fantasycalc_dynasty_rankings.csv", delim = ";") %>% 
  select(player_name = name, age, value, posRank = positionRank,
         fc_rank_overall = overallRank, trend30day)

ktc <- read_csv("final_data/ktc_values.csv") %>% 
  mutate(ktc_rank = row_number()) %>% 
  rename(ktc_value = player_values)

fc_map <- read_csv("mappings/fc_map.csv")
draft_data_map <- read_csv("mappings/draft_data_map.csv")

draft_data <- read_csv("final_data/pfr_draft_2010_2022.csv") %>% 
  clean_names() %>% 
  rename(draft_age = age, draft_year = year, player_name = player) %>% 
  filter(to == 2022) %>% 
  select(-to) %>% 
  left_join(draft_data_map) %>% 
  mutate(combined_name = coalesce(player_name_combined, player_name)) %>% 
  drop_na(combined_name) %>% 
  select(player_name = combined_name,
         draft_age, draft_year, pos, rnd, pick)

fp_tv <- read_csv("final_data/fantasy_pros_trade_value_march.csv") %>% 
  rename(player_name = name, fp_value = value_num)

fp_map <- read_csv("mappings/fp_mapping.csv")
sleeper_map <- read_csv("mappings/player_sleeper_team_map.csv")
sleeper_teams <- read_csv("final_data/reference_table/sleeper_teams.csv")

# map ktc names to fc
fc_mapped <- fantasycalc %>% 
  mutate(player_name = str_remove(.$player_name, "Rarrow_circle_up|Rarrow_circle_down|arrow_circle_up|arrow_circle_down|R$")) %>%
  left_join(fc_map) %>%
  mutate(combined_name = coalesce(ktc_name, player_name)) %>%
  select(player_name = combined_name, age, fc_value = value, fc_rank = posRank, fc_rank_overall)

fp_mapped <- fp_tv %>% 
  left_join(fp_map) %>% 
  mutate(combined_name = coalesce(player_name_combined, player_name)) %>% 
  select(player_name = combined_name, fp_value)

# Join together - include sleeper maps here as they are already consolidated
combined <- ktc %>% 
  left_join(fc_mapped) %>% 
  left_join(draft_data) %>% 
  left_join(fp_mapped) %>% 
  left_join(sleeper_map) %>% 
  left_join(sleeper_teams)

# Check what doesn't map (if needed) ---------
# no_join_fc <- anti_join(fantasycalc, ktc)
# no_join_ktc <- anti_join(ktc, fantasycalc)
# 
no_join_fc_map_list <- no_join_fc %>%
  filter(!str_detect(player_name, "202*"))
no_join_ktc_map_list <- no_join_ktc %>%
  filter(!str_detect(player_name, "202*"))

# If needed, we get this list to map against KTC
write_csv(no_join_fc_map_list, "mapping_files/fc_map_list.csv")

# calculate difference in value and rank
combined_analysis <- combined %>% 
  mutate(rank_diff = ktc_rank - fc_rank_overall,
         value_avg = (ktc_value + fc_value) / 2)

players_dropped <- filter(combined_analysis, is.na(value_avg))

combined_analysis_drop_na <- combined_analysis %>% 
  drop_na(value_avg) %>% 
  arrange(desc(value_avg)) %>% 
  mutate(avg_value_rank = row_number(),
         value_avg_drop = value_avg - lag(value_avg))

# range
ktc_max <- range(combined_analysis_drop_na$ktc_value)[2]
ktc_min <- range(combined_analysis_drop_na$ktc_value)[1]
ktc_data_range <- ktc_max - ktc_min

fc_max <- range(combined_analysis_drop_na$fc_value)[2]
fc_min <- range(combined_analysis_drop_na$fc_value)[1]
fc_range <- fc_max - fc_min

fp_min <- range(fp_tv$fp_value)[1]
fp_range <- range(fp_tv$fp_value)[2] - range(fp_tv$fp_value)[1]

normalized_values <- combined_analysis_drop_na %>%
  mutate(normalized_ktc = round((ktc_value - ktc_min) / ktc_data_range * 10000, 0),
         normalized_fc = round((fc_value - fc_min) / fc_range * 10000, 0),
         avg_norm = (normalized_ktc + normalized_fc) / 2) %>% 
  arrange(desc(avg_norm)) %>% 
  mutate(avg_norm_rank = row_number(),
         norm_value_drop = avg_norm - lag(avg_norm),
         fc_position = str_extract(fc_rank, "^[A-Z]{2}")) %>% 
  select(player_name, normalized_ktc, normalized_fc, ktc_rank, fc_position,
         fc_rank_overall, rank_diff, avg_norm, avg_norm_rank, norm_value_drop,
         draft_year, rnd, pick, team_name)

normalized_bi_values <- combined %>% 
  drop_na(fc_value) %>% 
  mutate(normalized_ktc = round((ktc_value - ktc_min) / ktc_data_range * 10000, 0),
         normalized_fc = round((fc_value - fc_min) / fc_range * 10000, 0),
         avg_norm = (normalized_ktc + normalized_fc) / 2,
         weighted_avg_norm = (normalized_ktc * .33) + (normalized_fc * .66)) %>% 
  arrange(desc(weighted_avg_norm)) %>% 
  mutate(weighted_avg_norm_rank = row_number(),
         weighted_norm_value_drop = weighted_avg_norm - lag(weighted_avg_norm),
         fc_position = str_extract(fc_rank, "^[A-Z]{2}")) %>% 
  select(player_name, normalized_ktc, normalized_fc, ktc_rank, fc_position,
         fc_rank_overall, avg_norm, weighted_avg_norm, weighted_avg_norm_rank, 
         weighted_norm_value_drop, draft_year, rnd, pick, team_name)
  
# Export final datasets       
write_csv(combined_analysis, "final_data/combined_analysis.csv")
write_csv(combined_analysis_drop_na, "final_data/combined_analysis_avg_value.csv")
write_csv(normalized_values, "final_data/combined_analysis_norm_values.csv")
write_csv(normalized_bi_values, "final_data/combined_analysis_weighted_bi_norm_values.csv")

# Make some charts!
ggplot(normalized_values, aes(x = norm_value_drop)) + 
  geom_histogram()

ggplot(combined_analysis_drop_na, aes(x = ktc_value, y = fc_value)) +
  geom_point()

top_50 <- combined_analysis_drop_na %>% 
  filter(avg_value_rank <= 50)

ggplot(top_50, aes(x = ktc_value, y = fc_value, label = avg_value_rank)) +
  geom_point() +
  geom_label()

# Try some k-means
cluster_df <- top_50 %>% 
  transmute(fc_value = as.numeric(fc_value), 
            ktc_value = as.numeric(ktc_value))

cluster_df <- na.omit(cluster_df)

k5 <- kmeans(cluster_df, centers = 5)

top_50 %>% 
  select(player_name, fc_value, ktc_value) %>% 
  as_tibble() %>% 
  mutate(cluster = k5$cluster) %>% 
  ggplot(aes(ktc_value, fc_value, color = factor(cluster), label = player_name)) +
  geom_point()

# again for tri values
# Make some charts!
ggplot(normalized_tri_values, aes(x = weighted_norm_value_drop)) + 
  geom_histogram()

ggplot(normalized_tri_values, aes(x = normalized_ktc, y = normalized_fc)) +
  geom_point()

top_50_norm <- normalized_tri_values %>% 
  filter(weighted_avg_norm_rank <= 50)

ggplot(top_50_norm, aes(x = normalized_ktc, y = normalized_fc, label = weighted_avg_norm_rank)) +
  geom_point() +
  geom_label()

# Try some k-means
cluster_df_norm <- top_50_norm %>% 
  transmute(fc_value = as.numeric(normalized_fc), 
            ktc_value = as.numeric(normalized_ktc),
            fp_value = as.numeric(normalized_fp))

cluster_df_norm <- na.omit(cluster_df_norm)

k5_norm <- kmeans(cluster_df_norm, centers = 7)

top_50_norm %>% 
  select(player_name, normalized_fc, normalized_ktc) %>% 
  as_tibble() %>% 
  mutate(cluster = k5_norm$cluster) %>% 
  ggplot(aes(normalized_ktc, normalized_fc, color = factor(cluster), label = player_name)) +
  geom_point()
