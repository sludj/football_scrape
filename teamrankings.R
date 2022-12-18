################################################################################
#
# Title: Scrape of teamrankings.com
#
# Created: 12/18/22 (CT)
#
# Steps: 1. Libraries and setup
#        2. Rvest pull of main table
#        3. Analysis
#
################################################################################

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(rvest)

# Step 2. Rvest pull of main table ---------------------------------------------
url <- "https://www.teamrankings.com/nfl/stat/yards-per-play"
  
main_table <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table()

table_df <- main_table[[1]]

# Step 3. Export & share -------------------------------------------------------
write_csv(table_df, "final_data/table_df.csv")
