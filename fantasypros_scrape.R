# attempt to scrape fantasypros.com
# Only requirement is installing these packages and java
library("RSelenium")
library("rvest")
library("tidyverse")

# This installs some stuff and opens our browser
driver <- rsDriver(browser = "firefox",
                   chromever = NULL)

# Assign the client (browser) to an object for us to use (send commands)
remDr <- driver[["client"]]

# Navigate in the new browser to fantasycalc
remDr$navigate("https://www.fantasypros.com/nfl/stats/dst.php")

# Step 2: Get the html scrape of page 1
# Allow the webpage to load
Sys.sleep(5)

output <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(output) <- c("Rank", "Player", "SACK", "INT", "FR", "FF", "DEF TD", 
                      "SFTY", "SPC TD", "G", "FPTS", "FPTS/G", "ROST", "year")

for(i in seq(2, 12)) {
  html <- remDr$getPageSource()[[1]]
  
  player_rankings <- read_html(html) %>% 
    html_nodes("table") %>% 
    html_table()
  
  player_rankings_df <- player_rankings[[1]]
  
  title <- read_html(html) %>% 
    html_nodes("title") %>% 
    html_text()
  
  year <- substr(title, 1, 4)
  
  player_rankings_df_year <- player_rankings_df %>% 
    mutate(year = year)
  
  output <- rbind(output, player_rankings_df_year)
  
  dropdown <- remDr$findElement(using = "xpath", "/html/body/div[2]/div[4]/div/div/div/div[5]/div/form/div[1]/select")
  dropdown$clickElement()
  
  Sys.sleep(5)
  
  remDr$findElement(using = "xpath", paste0("/html/body/div[2]/div[4]/div/div/div/div[5]/div/form/div[1]/select/option[", i, "]"))$clickElement()
  
  Sys.sleep(5)
  }

avg_points_by_team <- output %>% 
  group_by(Player) %>% 
  summarize(avg_pts = mean(FPTS))

ggplot(output, aes(x = year, y = `FPTS/G`, label = Player)) +
  geom_boxplot()

output_5_yr <- output %>% 
  filter(year >= 2017)

ggplot(output_5_yr, aes(x = year, y = `FPTS/G`, label = Player)) + 
  geom_dotplot(binaxis = "y", stackdir = "center")

write_csv(output, "final_data/fantasypros_def_values.csv")

remDr$close()
gc()
