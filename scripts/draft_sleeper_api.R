# Pull from sleeper
#

library(tidyverse)
library(httr)
library(jsonlite)

raw_sleeper <- GET("https://api.sleeper.app/v1/league/919635179260526592/rosters")

char_sleeper <- fromJSON(rawToChar(raw_sleeper$content))

raw_sleeper_players <- GET("https://api.sleeper.app/v1/players/nfl")

char_sleeper_players <- fromJSON(rawToChar(raw_sleeper_players$content))

df_sleeper_players <- do.call(what = "rbind",
                              args = lapply(char_sleeper_players, as.data.frame))
