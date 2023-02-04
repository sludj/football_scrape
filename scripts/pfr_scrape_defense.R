# Scraping some pro-football-reference data
# 
# Goal is to get defense analysis

# Step 1: Libraries and setup --------------------------------------------------
library(tidyverse)
library(rvest)

# Step 2. Rvest pull of main table ---------------------------------------------
# We want the table for the past 10 years
years_to_pull <- seq(from = 2000, to = 2022, by = 1)
output <- data.frame(matrix(ncol = 29, nrow = 0))
colnames(output) <- c("Rk", "Tm", "G", "PA", "Yds", "Tot Yds & TO Ply", 
                      "Tot Yds & TO Y/P", "Tot Yds & TO TO", "FL", "1stD",
                      "Passing Cmp", "Passing Att", "Passing Yds", "Passing TD",
                      "Passing Int", "Passing NY/A", "Passing 1stD", "Rushing Att",
                      "Rushing Yds", "Rushing TD", "Rushing Y/A", "Rushing 1stD",
                      "Penalties Pen", "Penalties Yds", "Penalties 1stPy", "Sc%",
                      "TO%", "EXP", "year")

for (i in seq(1, 22)) {
  
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
  
  # sleep to avoid too many requests
  Sys.sleep(5)
}

# Quick analysis of the def
output$Rk <- as.numeric(output$Rk)
output$year <- as.date(output$year)

output_avg_rnk <- output %>% 
  group_by(Tm) %>% 
  summarize(avg_rank = mean(Rk))

ggplot(output, aes(x = year, y = Rk)) +
  geom_line(aes(color = Tm))

# Step 3. Export & share -------------------------------------------------------
write_csv(output, "final_data/table_df.csv")
