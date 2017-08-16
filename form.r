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
    select(team, field, PPG, games, Pts) %>% #TODO remove Pts ?
    arrange(field, -PPG)
}

#calculate from on PPG for a team in selected season
calc_form_ppg <- function(tm, fld, season_state) {
  oppField <- ifelse(fld == 'A', 'H', 'A')
  last_n_games <- season_state$data %>%
    filter(field == fld & team == tm & Div == season_state$Div & Date > season_state$fromDate) %>%
    top_n(season_state$num_games, Date)
  form_result <- 
    left_join(data.frame(team = last_n_games$opp, field = rep(oppField, season_state$num_games)), 
              season_state$season_form,
              by = c("team", "field")) %>%
    full_join(., 
              last_n_games, 
              by = c("team" = "opp"), 
              suffix = c(".opp.form", ".team"))
  sum (form_result$Pts.team * form_result$PPG)
}
  

#current form, calculated from latest games and ppg of those oppenents
form_ppg <- function(data, num_games, fromDate, toDate, teams) {
  season_state <- list()
  season_state$num_games <- num_games #either home or away
  season_state$all_teams <- unique(as.data.frame(data %>% filter (Div == "E0" & Date > fromDate & Date < toDate))[,"team"])
  season_state$season_form <- ppg_pl(data, fromDate, toDate, all_teams)
  season_state$data <- data
  season_state$fromDate <- fromDate
  season_state$toDate <- toDate
  season_state$Div <- "E0"

  rbind(
    do.call("rbind", lapply(teams, calc_form_ppg, 'A', season_state)),
    do.call("rbind", lapply(teams, calc_form_ppg, 'H', season_state))
  )
}

form_ppg(data, num_games, fromDate, toDate, c("Liverpool", "Watford"))
