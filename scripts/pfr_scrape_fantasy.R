# Scraping some pro-football-reference data
# 
# Goal is to get player information
# We want fantasy stats by year, and combine stats to join on the player

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(rvest)

# Step 2. Rvest pull of main table ---------------------------------------------
# We want the table for the past 10 years
years_to_pull <- seq(from = 2000, to = 2022, by = 1)
output <- data.frame(matrix(ncol = 34, nrow = 0))

# list of all columns we're going to pull
colnames(output) <- c("Rk", "Player", "Tm", "FantPos", "Age", "Games G", "Games GS",
                      "Passing Cmp", "Passing Att", "Passing Yds", "Passing TD",
                      "Passing Int", "Rushing Att", "Rushing Yds", "Rushing Y/A",
                      "Rushing TD", "Receiving Tgt", "Receiving Rec", "Receiving Yds",
                      "Receiving Y/R", "Receiving TD", "Fumbles Fmb", "Fumbles FL",
                      "Scoring TD", "Scoring 2PM", "Scoring 2PP", "Fantasy FantPt",
                      "Fantasy PPR", "Fantasy DKPt", "Fantasy FDPt", "Fantasy VBD",
                      "Fantasy PosRank", "Fantasy OvRank", "year")

# Pull for each year in our list
for (i in seq(1, 22)) {
  
  url <- sprintf("https://www.pro-football-reference.com/years/%s/fantasy.htm", years_to_pull[i])
  
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
  
  table_df$Rk <- as.numeric(table_df$Rk)
  
  table_df_year <- table_df %>% 
    mutate(year = years_to_pull[i]) %>% 
    # This doesn't work, blanks are not being counted as NA, probably want to update that
    drop_na(Rk)
  
  output <- rbind(output, table_df_year)
  
  # sleep to avoid too many requests
  Sys.sleep(5)
}

# Clean it up
# Start with the extra character in the output player
fantasy_clean_names <- output %>% 
  mutate(Player = str_remove_all(.$Player, "[^[:alpha:]]+$"))

distinct(fantasy_clean_names, Player)

