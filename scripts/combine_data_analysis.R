# libraries
library(tidyverse)
library(ggplot2)

# Combine the data
fantasycalc <- read_csv("final_data/fantasycalc_values.csv") %>% 
  mutate(fc_rank_overall = row_number(),
         fc_position = str_sub(posRank, end = 2))

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
  mutate(avg_value_rank = row_number(),
         value_avg_drop = value_avg - lag(value_avg))

write_csv(combined_analysis, "final_data/combined_analysis.csv")
write_csv(combined_analysis_drop_na, "final_data/combined_analysis_avg_value.csv")

# Make some charts!
ggplot(combined_analysis_drop_na, aes(x = value_avg_drop)) + 
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
