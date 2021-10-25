library(NBAr)
library(tidyverse)

#returns playerbio of individual player (player_id, player_name, team_id, team_abbreviation,
#age, player_height, palyer_height_inches, player_weight, college, country, draft_year, 
#draft_round, draft_number, gp, pts, reb, ast, net_rating, oreb_pct, dreb_pct, usg_pct,
#ts_pct, ast_pct)

get_oneplayer <- function (year, playerName){
  playerbio = get_playerbio(season = year)
  for (i in 1:nrow(playerbio)){
    if (tolower(playerbio[i,2])==tolower(playerName)){
      return (playerbio[i,])
    }
  }
}

