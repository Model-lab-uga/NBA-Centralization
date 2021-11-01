#In order to install this package you must have devtools installed. To do so type this into your console: install.packages("devtools")

#Once devtools is installed you will need to pull this package from github by typing this into your console: devtools::install_github("rtelmore/ballr")

#Once this is installed all you have to do is include NBAr in your library like I have done below by running: library(ballr)

#These functions also require you to have the tidyverse library installed as well


#NBAPerGameAdvStatistics ()
## @param season A numeric year
players <- NBAPerGameAdvStatistics(season = 2018)

##This function returns all player advanced statistics for each player from an NBA 
##season on basketball-reference.com. See an example table at: 
##https://www.basketball-reference.com/leagues/NBA_2018_advanced.html




#NBAPerGameStatistics()
## @param season A numeric year
players2 <- NBAPerGameStatistics(season = 2018)

##This function returns all player statistics on a per game basis 
##from an NBA season on basketball-reference.com. See an example table at: 
##http://www.basketball-reference.com/leagues/NBA_2015_per_game.html




##NBAPerGameStatisticsPer100Poss()
## @param season A numeric year
players3 <- NBAPerGameStatisticsPer100Poss(season = 2018)

##This function returns all player statistics per 100 possessions for each player 
##from an NBA season on basketball-reference.com. See an example table at: 
##https://www.basketball-reference.com/leagues/NBA_2018_per_poss.html




##NBAPerGameStatisticsPer36Min()
## @param season A numeric year
players4 <- NBAPerGameStatisticsPer36Min(season = 2018)

##This function returns all player statistics on a per game basis (per 36 min) 
##from an NBA season on basketball-reference.com. See an example table at:
##http://www.basketball-reference.com/leagues/NBA_2018_per_minute.html




##NBAPlayerPerGameStats(p)
## @param player_link A link suffix, e.g. "/players/d/davisan02.html"
james_career <- NBAPlayerPerGameStats("/players/j/jamesle01.html") # Lebron James

##This function gets a player's career stats from basketball-reference.com





##NBASeasonTeamByYear()
## @param team	One of "ATL", "BOS", etc.
## @param season  The particular season you are querying
ATL_2018_season <- NBASeasonTeamByYear("ATL", 2018)

##This function returns a team's yearly statistics from basketball-reference.com




##NBAStandingsByDate()
## @param date_string	A String of the form "2015-04-01"
standings <- NBAStandingsByDate("2010-01-31") # Jan 31, 2010

##This function returns An list containing the standings in the 
##Eastern and Western Conferences from the given date






