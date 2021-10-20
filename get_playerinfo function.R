library(NBAr)
library(tidyverse)

get_playerinfo <- function (year, playerName){
  playerbio = get_playerbio(season = year)
  for (i in 1:nrow(playerbio)){
    if (tolower(playerbio[i,2])==tolower(playerName)){
      return (playerbio[i,])
    }
  }
}

