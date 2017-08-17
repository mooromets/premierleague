source("./newcomers_performance.r")

MISSED_GAMES <- 3 # a team can miss some due to cups and other circumstances

EPL1516 <- list(div = "E0"
                , fromDate = ymd("20150801")
                , toDate = ymd("20160801"))

PLnewcom <- list(num_games = 38
                 , points_home = 25.07407 # average points, collected at home by a newcommer 
                 , points_away = 13.7037 # average points, collected away by a newcommer
                )

## Points Per Game in competition
ppg <- function(data, competition, newcomStats, teams) {
  ppg_res <- full_join(
    as.data.frame(points(data, competition$fromDate, competition$toDate, competition$div, teams)), # this data.frame 
    # potentially misses some teams (newcomers, that didn't play last year), 
    # so we join results with empty data frame
    data.frame(team = rep(teams,2), 
               field = c(rep('H',length(teams)), rep('A',length(teams)))),
    by = c("team", "field")
    )
  ppg_res[is.na(ppg_res)] <- 0
  mean_games <- round(mean(ppg_res[ppg_res$games != 0, "games"]))
  ppg_res[ppg_res$games<mean_games-MISSED_GAMES & ppg_res$field=='H', c("Pts", "games")] <-
    c(round(newcomStats$points_home/newcomStats$num_games/2*mean_games), mean_games)
  ppg_res[ppg_res$games<mean_games-MISSED_GAMES & ppg_res$field=='A', c("Pts", "games")] <-
    c(round(newcomStats$points_away/newcomStats$num_games/2*mean_games), mean_games)
  #TODO: not simply replace small values, but append averages to existing Pts and games
  ppg_res %>%
    mutate(PPG = Pts / games) %>%
    select(team, field, PPG, games, Pts) %>% #TODO remove Pts ?
    arrange(field, -PPG)
}

#calculate from on PPG for a team in selected season
calc_form_ppg <- function(tm, fld, data, competition, competition_state) {
  oppField <- ifelse(fld == 'A', 'H', 'A')
  last_n_games <- data %>%
    filter(field == fld & team == tm & Div == competition$div & Date > competition$fromDate) %>%
    top_n(competition_state$num_games, Date)
  form_result <- 
    left_join(data.frame(team = last_n_games$opp, field = rep(oppField, competition_state$num_games)), 
              competition_state$form,
              by = c("team", "field")) %>%
    full_join(., 
              last_n_games, 
              by = c("team" = "opp"), 
              suffix = c(".opp.form", ".team"))
  sum (form_result$Pts.team * form_result$PPG)
}
  

#current form, calculated from latest games and ppg of those oppenents
form_ppg <- function(data, num_games, competition, teams) {
  competition_state <- list()
  competition_state$num_games <- num_games #either home or away
  competition_state$all_teams <-
    unique(as.data.frame(data %>% 
                           filter (Div == competition$div & 
                                     Date > competition$fromDate 
                                   & Date < competition$toDate)
                         )[,"team"])
  competition_state$form <- ppg(data, competition, PLnewcom, all_teams)

  rbind(
    do.call("rbind", lapply(teams, calc_form_ppg, 'A', data, competition, competition_state)),
    do.call("rbind", lapply(teams, calc_form_ppg, 'H', data, competition, competition_state))
  )
}

t1 <- form_ppg(data, 5, EPL1516, c("Liverpool", "Watford", "Chelsea", "Leicester"))
