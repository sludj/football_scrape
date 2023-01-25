# Attempt to pull from fantasycalc.com/rankings
#
# Created: 1/19/23 (CT)
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
url <- "https://keeptradecut.com/dynasty-rankings?page=0&filters=QB|WR|RB|TE|RDP"

player_name <- url %>% 
  read_html() %>% 
  html_nodes("div.player-name") %>% 
  html_text() %>% 
  str_sub(25) %>% 
  str_remove("[A-Z]{2,}\\\r\\\n\\s*") %>% 
  str_remove("\\s*\\\r\\\n\\s*")

player_values <- url %>% 
  read_html() %>% 
  html_nodes("div.value") %>% 
  html_text() %>% 
  str_remove("\\\r\\\n\\s*") %>% 
  str_remove("\\\r\\\n\\s*")

player_data <- data.frame(
  player_name,
  player_values
)

final_output <- player_data[-1,]

# Step 3. Export & share -------------------------------------------------------
write_csv(final_output, "final_data/ktc_values.csv")
