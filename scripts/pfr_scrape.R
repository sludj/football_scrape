# Scraping some pro-football-reference data
# 
# Goal is to get player information
# We want fantasy stats by year, and combine stats to join on the player
# Also interested in some defense analysis

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(rvest)

# Mapping players
player_names <- read_csv("mappings/player_names.csv")

# Step 2. Rvest pull of main table ---------------------------------------------
# We want the table for the past 10 years
years_to_pull <- seq(from = 2000, to = 2022, by = 1)
output <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(output) <- c("Player", "Rnd", "Pick", "Age", "To", "draft_year")

for (i in seq(1, 22)) {
  
  url <- sprintf("https://www.pro-football-reference.com/years/%s/draft.htm", years_to_pull[i])
  
  main_table <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table()
  
  table_df <- main_table[[1]]
  
  columns <- paste(names(table_df), as.matrix(table_df[1,])) %>% 
    str_trim()
  
  colnames(table_df) <- columns
  
  # Drop extra first row we used for names
  table_df <- table_df[-1,]
  
  table_df_year <- table_df %>% 
    select(Player, Rnd, Pick, Age, To) %>% 
    mutate(year = years_to_pull[i]) %>% 
    # This doesn't work, blanks are not being counted as NA, probably want to update that
    filter(Player != "Player")
  
  output <- rbind(output, table_df_year)
  
  # sleep to avoid too many requests
  Sys.sleep(5)
}

# Check join with player list
anti_join(player_names, output)

# Step 3. Export & share -------------------------------------------------------
write_csv(output, "final_data/pfr_draft_2000_2022.csv")
