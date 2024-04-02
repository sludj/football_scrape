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
remDr$navigate("https://www.fantasycalc.com/rankings")

# Step 2: Get the html scrape of page 1
# Allow the webpage to load
Sys.sleep(5)

# Click the button to turn it to superflex rankings
# comment it as maybe defaulting to superflex now
#remDr$findElement(using = "xpath", "/html/body/app-root/main/app-rankings/div/div[1]/div/div[1]/app-single-settings-select/div/div/ui-switch/button")$clickElement()

# Ensure it loaded superflex
Sys.sleep(5)

output <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(output) <- c("player_name", "age", "value", "posRank")

for(i in seq(1, 7)) {
  html <- remDr$getPageSource()[[1]]
  
  player_rankings <- read_html(html) %>% 
    html_nodes("table") %>% 
    html_table()
  
  player_rankings_df <- player_rankings[[1]]
  
  player_rankings_df_clean <- player_rankings_df %>% 
    select(player_name = Name, age = Age, value = Value)
  
  player_rankings_final <- player_rankings_df_clean %>% 
    mutate(posRank = str_extract(.$player_name, "[A-Z]{2,}\\d{1,}"),
           player_name = str_remove(.$player_name, "[A-Z]{2,}\\d{1,}"))
  
  output <- rbind(output, player_rankings_final)
  
  remDr$findElement(using = "xpath", "/html/body/app-root/main/app-dynasty-rankings/app-rankings/div/div[1]/div[2]/app-rankings-list/mat-card/mat-card-content/mat-paginator/div/div/div[2]/button[2]")$clickElement()
}

write_csv(output, "final_data/fantasycalc_values.csv")

remDr$close()
gc()
