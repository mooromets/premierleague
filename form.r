source("./newcomers_performance.r")

PL_GAMES <- 19 # number of  HOME-games in a season (or 'away', as well)
NEWBOY_HOME <- 25.07407 # average points, collected at HOME by a newcommer 
NEWBOY_AWAY <- 13.7037 # average points, collected AWAY by a newcommer 
MISSED_GAMES <- 3 # a team can miss some due to cups and other circumstances

## Points Per Game in Premier League
ppg_pl <- function(data, fromDate, toDate, teams) {
  ppg <- full_join(
    as.data.frame(points(data, fromDate, toDate, "E0", teams)), # this data.frame 
    # potentially misses some teams (newcomers, that didn't play last year), 
    # so we join results with empty data frame
    data.frame(team = rep(teams,2), 
               field = c(rep('H',length(teams)), rep('A',length(teams)))),
    by = c("team", "field")
    )
  ppg[is.na(ppg)] <- 0
  mean_games <- round(mean(ppg[ppg$games != 0, "games"]))
  ppg[ppg$games<mean_games-MISSED_GAMES & ppg$field=='H', c("Pts", "games")] <-
    c(round(NEWBOY_HOME/PL_GAMES*mean_games), mean_games)
  ppg[ppg$games<mean_games-MISSED_GAMES & ppg$field=='A', c("Pts", "games")] <-
    c(round(NEWBOY_AWAY/PL_GAMES*mean_games), mean_games)
  #TODO: not simply replace small values, but append averages to existing Pts and games
  ppg %>%
    mutate(PPG = Pts / games) %>%
    select(team, field, PPG, games) %>%
    arrange(field, -PPG)
}