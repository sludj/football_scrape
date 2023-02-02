# attempt to scrape fantasycalc.com
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

output <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(output) <- c("Rank", "Player", "SACK", "INT", "FR", "FF", "DEF TD", 
                      "SFTY", "SPC TD", "G", "FPTS", "FPTS/G", "ROST")

for(i in seq(1, 7)) {
  html <- remDr$getPageSource()[[1]]
  
  player_rankings <- read_html(html) %>% 
    html_nodes("table") %>% 
    html_table()
  
  player_rankings_df <- player_rankings[[1]]
  
  output <- rbind(output, player_rankings_df)
  
  remDr$findElement(using = "xpath", "/html/body/div[2]/div[4]/div/div/div/div[5]/div/form/div[1]/select")$clickElement()
}

write_csv(output, "final_data/fantasycalc_values.csv")

remDr$close()
gc()
