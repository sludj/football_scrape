################################################################################
#
# Title: Scrape of fantasy pros trade values
#
# Created: 2/12/23 (CT)
#
# Steps: 1. Libraries and setup
#        2. Rvest pull of main table
#        3. Analysis
#
# Could not get it to work with selenium or basic scrape. They have a download
# on the site though and doesn't look to be updated frequently. Will use that
#
################################################################################

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(janitor)

# Import the fp files
files <- list.files(path = "raw_data/", pattern = "fp.*csv") %>% 
  map_df(~read_csv(paste0("raw_data/", .), col_types = "ccccc")) %>% 
  clean_names() %>% 
  mutate(value = coalesce(.$sf_value, .$trade_value),
         value_num = as.numeric(value)) %>% 
  select(name, value_num) %>% 
  filter(value_num > 1) %>% 
  arrange(desc(value_num))

# export
write_csv(files, "final_data/fantasy_pros_trade_value_march.csv")


         