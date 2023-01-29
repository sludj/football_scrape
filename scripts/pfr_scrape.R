# Scraping some pro-football-reference data
# 
# Goal is to get player information
# We want fantasy stats by year, and combine stats to join on the player
# Also interested in some defense analysis

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(rvest)

# Step 2. Rvest pull of main table ---------------------------------------------
# We want the table for the past 10 years
years_to_pull <- seq(from = 2012, to = 2022, by = 1)
output <- data.frame(matrix(ncol = 29, nrow = 0))
colnames(output) <- c("Rk", "Tm", "G", "PA", "Yds", "Tot Yds & TO Ply", 
                      "Tot Yds & TO Y/P", "Tot Yds & TO TO", "FL", "1stD",
                      "Passing Cmp", "Passing Att", "Passing Yds", "Passing TD",
                      "Passing Int", "Passing NY/A", "Passing 1stD", "Rushing Att",
                      "Rushing Yds", "Rushing TD", "Rushing Y/A", "Rushing 1stD",
                      "Penalties Pen", "Penalties Yds", "Penalties 1stPy", "Sc%",
                      "TO%", "EXP", "year")

for (i in seq(1, 10)) {
  
  url <- sprintf("https://www.pro-football-reference.com/years/%s/opp.htm", years_to_pull[i])
  
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
    mutate(year = years_to_pull[i]) %>% 
    # This doesn't work, blanks are not being counted as NA, probably want to update that
    drop_na(Rk)
  
  output <- rbind(output, table_df_year)
}


# Step 3. Export & share -------------------------------------------------------
write_csv(table_df, "final_data/table_df.csv")
