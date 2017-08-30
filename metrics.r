library(dplyr)
library(lubridate)

MISSED_GAMES <- 3 # a team can miss some due to cups and other circumstances

#sample 'distance' structure
distance1516 <- list(
  div = "E0"
  ,period =  c(ymd("20150801"), ymd("20160801"))
)


#sample 'rules' structure
rulesEPL <- list(
  num_games = 38 # 1 season in EPL
  , new_home_pts = 25.07 # average home points in EPL for a newcommer 
  , new_away_pts = 13.70 # average away points in EPL for a newcommer
  , months = 9 # season's length in months
)


# calculate points in a long distance (= a cross)
points_cross <- function (data, teams, distance) {
  data %>%
    #filter by input params
    filter(Div == distance$div, 
           between(Date, distance$period[1], distance$period[2]), 
           team %in% teams) %>%
    group_by(team, field) %>%
    summarise(Pts = sum(Pts), games = n()) %>%
    arrange(-Pts)
}


# calculate points per game (PPG) in a long distance, (= a speed of a cross)
points_cross_speed <- function(data, teams, distance, rules) {
  # get points_cross data
  speed <- full_join(
    as.data.frame(points_cross(data, teams, distance)), # this data.frame 
    # potentially misses some teams (newcomers, that didn't play last year), 
    # so we join results with empty data frame
    data.frame(team = rep(teams, 2), 
               field = c(rep('H',length(teams)), rep('A',length(teams)))),
    by = c("team", "field")
  )
  
  # fill in points for teams with NA or 0 points (newcommers)
  speed[is.na(speed)] <- 0
  mean_games <- round(mean(speed[speed$games != 0, "games"]))
  newbie_pts <- data.frame(H = c(rules$new_home_pts), A = c(rules$new_away_pts))
  for (side in colnames(newbie_pts) ) {
    speed[speed$games < mean_games - MISSED_GAMES & 
            speed$field == side, c("Pts", "games")] <-
      c(round(newbie_pts[side] / rules$num_games / 2 * mean_games), mean_games)
  }
  #TODO:  not simply replace pts with small number of games, but append averages 
  #       to existing Pts and games
  
  speed %>%
    mutate(PPG = Pts / games) %>%
    select(team, field, PPG, games, Pts) %>% #TODO remove Pts ?
    arrange(field, -PPG)
}


# calculate relative speed for a single team
one_team_ppg <- function(fld, tm, data, distance, competition_state, rules) {
  oppField <- ifelse(fld == 'A', 'H', 'A')
  # last n games must be without gaps (breaks or midseasons), so we calculate
  # the earliest day we accept for relative PPG calculation
  months_for_n_games <- floor(competition_state$num_games / 
                                (rules$num_games / 2 / rules$months)) + 1
  earliestDate <- ymd(paste(c(year(distance$period[2]), 
                          month(distance$period[2]) - months_for_n_games, 
                          day(distance$period[2])), 
                        collapse = "-"))
  last_n_games <- data %>%
    filter(field == fld & team == tm & Div == distance$div & Date < distance$period[2] 
           & Date > earliestDate) %>%
    top_n(competition_state$num_games, Date)
  # check if there's enough games to calc relative PPG
  if (dim(last_n_games)[1] >= competition_state$num_games) {
    # calc opposition's performance in the last n games
    form_result <-  
      left_join(data.frame(team = last_n_games$opp, 
                           field = rep(oppField, competition_state$num_games)), 
                competition_state$form,
                by = c("team", "field")) %>%
      full_join(., 
                last_n_games, 
                by = c("team" = "opp"), 
                suffix = c(".opp.form", ".team"))
  } else 
    form_result <- data.frame(Pts.team = NA, PPG = NA)
  list (team = tm, 
        field = fld,
        ppg_rel = mean (form_result$Pts.team * form_result$PPG))
}


# calculates PPG relatively to other teams' performance (= relative speed)
points_sprint_relative_speed <- 
  function(data, num_games, div, toDate, teams, rules) {
  distance <- 
    list(div = div, period =  c(ymd(paste(c(year(toDate)-1, 
                                            month(toDate), 
                                            day(toDate)), collapse = "-"))
                                , toDate))
  competition_state <- list()
  competition_state$num_games <- num_games #either home or away
  competition_state$all_teams <-
    as.vector(
      unique(as.data.frame(data %>% 
                             filter (Div == distance$div & 
                                       Date > distance$period[1] 
                                     & Date < distance$period[2])
      )[,"team"])
    )
  competition_state$form <- 
    points_cross_speed(data, competition_state$all_teams, distance, rules)

  # relative speed for a single team HOME and AWAY
  one_team_ppg_ha <- function(team) {
    do.call("rbind", lapply(list('A', 'H'), one_team_ppg, team, data, distance, 
                            competition_state, rules))
  }
  
  do.call("rbind", lapply(teams, one_team_ppg_ha))
}