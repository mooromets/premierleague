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
)


points_cross <- function (data, teams, distance) {
# calculate points in a long distance
  data %>%
    #filter by input params
    filter(Div == distance$div, 
           between(Date, distance$period[1], distance$period[1]), 
           team %in% teams) %>%
    group_by(team, field) %>%
    summarise(Pts = sum(Pts), games = n()) %>%
    arrange(-Pts)
}

points_speed <- function(data, teams, distance, rules) {
# calculate points per game in a long distance, aka speed = distance/time
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

#TODO : add points acceleration
