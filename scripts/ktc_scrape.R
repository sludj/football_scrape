# Attempt to pull from keeptradecut.com/dynasty-rankings
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

output <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(output) <- c("player_name", "player_values")

for(i in seq(from = 0, to = 9, by = 1)) {
  url <- paste0("https://keeptradecut.com/dynasty-rankings?page=", i, "&filters=QB|WR|RB|TE|RDP")
  
  player_name <- url %>% 
    read_html() %>% 
    html_nodes("div.player-name") %>% 
    html_text() %>% 
    str_sub(25) %>% 
    str_remove("[A-Z]{2,}\\\r\\\n\\s*") %>% 
    str_remove("\\s*\\\r\\\n\\s*") %>% 
    str_remove("\\s*") %>% 
    str_remove("\\n\\s*") %>% 
    str_remove(".{3}$")
  
  player_values <- url %>% 
    read_html() %>% 
    html_nodes("div.value") %>% 
    html_text() %>% 
    str_remove("\\\r\\\n\\s*") %>% 
    str_remove("\\\r\\\n\\s*") %>% 
    str_remove("\\n\\s*") %>% 
    str_remove("\\n\\s*$")
  
  
  player_data <- data.frame(
    player_name,
    player_values
  )
  
  final_output <- player_data[-1,]
  
  output <- rbind(output, final_output)
}

# Step 3. Export & share -------------------------------------------------------
write_csv(output, "final_data/ktc_values.csv")

# Old scrape prior to weird changes
# url <- "https://keeptradecut.com/dynasty-rankings?page=0&filters=QB|WR|RB|TE|RDP"
# 
# player_name <- url %>% 
#   read_html() %>% 
#   html_nodes("div.player-name") %>% 
#   html_text() %>% 
#   str_sub(25) %>% 
#   str_remove("[A-Z]{2,}\\\r\\\n\\s*") %>% 
#   str_remove("\\s*\\\r\\\n\\s*") %>% 
#   str_remove("\\s*") %>% 
#   str_remove("\\n\\s*") %>% 
#   str_remove(".{3}$")
# 
# player_values <- url %>% 
#   read_html() %>% 
#   html_nodes("div.value") %>% 
#   html_text() %>% 
#   str_remove("\\\r\\\n\\s*") %>% 
#   str_remove("\\\r\\\n\\s*") %>% 
#   str_remove("\\n\\s*") %>% 
#   str_remove("\\n\\s*$")
#   
# 
# player_data <- data.frame(
#   player_name,
#   player_values
# )
# 
# final_output <- player_data[-1,]
