source("./lib/download.R")
source("./lib/read_data.R")
library(lubridate)

# returns all home games for the selected team in [startDate; endDate)
teamHomeGames <- function(data, teamName, startDate, endDate)
{
  filter(data, HomeTeam == teamName & Date >= startDate & Date < endDate) %>% 
    mutate(F = FTHG
           , A = FTAG
           , Res = ifelse(FTR == 'H', 'W', ifelse(FTR == 'A', 'L', 'D'))
    )  
}

# returns all away games for the selected team in [startDate; endDate)
teamAwayGames <- function(data, teamName, startDate, endDate)
{
  filter(data, AwayTeam == teamName & Date >= startDate & Date < endDate) %>% 
    mutate(F = FTAG
           , A = FTHG
           , Res = ifelse(FTR == 'A', 'W', ifelse(FTR == 'H', 'L', 'D'))
    )  
}

# calculates team's performance and returns single-row data.frame with data:
#("team", "Games", "W", "D", "L", "F", "A", "Diff" , "Pts", "avg.Scored", "avg.Conceeded", "avg.Points")   
teamPerformance <- function(data, team, startDate, endDate)
{
  # get games
  games_list <- bind_rows(
    teamHomeGames(data, team, startDate, endDate)
    , teamAwayGames(data, team, startDate, endDate)
  )
  
  #count won-drawn-lost
  res_count <- count(games_list, Res)
  
  # tranpose res_count data.frame
  wdl = c('W', 'D', 'L')
  d <- t(data.frame(t(left_join(data.frame(Res = wdl), res_count))[2,]))
  d[is.na(d)] <- 0
  d <- as.integer(d)
  d <- as.data.frame(t(data.frame(d)))
  colnames(d) <- wdl
  res_count <- d
  
  # count goals
  goals_count <- games_list %>%
    summarise(Games = n()
              , F = sum(F)
              , A = sum(A)
              , Diff = sum(F-A))

  # name of a team  
  teamDF <- data.frame(team = team)
  
  # all together
  all <- bind_cols(teamDF, res_count, goals_count)
  
  # additional data
  all <- mutate(all,
                Pts = W * 3 + D,
                avg.Points = round(Pts / Games, 2),
                avg.Scored = round(F / Games, 2),
                avg.Conceeded = round(A / Games, 2))

  # rearrange columns
  select(all, team, Games, W, D, L, F, A, Diff, Pts, avg.Scored, avg.Conceeded, avg.Points)
}

#print performance
print_performance <- function(team, data)
{
  startDate <- ymd(team['startDate'])
  endDate <- team['endDate']
  tmp <- teamPerformance(data, team['team'], startDate, endDate)
  tmp['team'] <- paste0(as.character(team['team']), 
                        " ",  
                        as.character(day(startDate)),
                        ".",
                        as.character(month.abb[month(startDate)]),
                        " - ",
                        as.character(day(endDate)),
                        ".",
                        as.character(month.abb[month(endDate)]))
  tmp
}


#################33
downloadSeasons(2019:2019,1)
data <- readSeasonsData(2019:2019,1)

# only needed columns 
data <- data[ , c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]

# convert string date
data <- mutate(data,
               Date = dmy(Date) # character to 
               # year - year of the season's end 
               #, year = ifelse(month(Date) > 7, year(Date)+1, year(Date))
               )

# teams list teams <- unique(c(as.character(data$HomeTeam), as.character(data$AwayTeam)))

pool <- data.frame(team = c("Liverpool", "Liverpool", "Liverpool"),
                 startDate = c("2018-08-10", "2018-10-30", "2018-12-31"),
                 endDate = c("2018-10-30", "2018-12-31", "2019-03-05"))

x <- bind_rows(apply(pool, 1, print_performance, data))

city <- data.frame(team = c("Man City", "Man City", "Man City"),
                   startDate = c("2018-08-10", "2018-10-30", "2018-12-31"),
                   endDate = c("2018-10-30", "2018-12-31", "2019-03-05"))

xCity <- bind_rows(x, apply(city, 1, print_performance, data))


###################################33
# sliding frame

nGamesTeamPerformance <- function(N, teamName)
{
  gameDates <- data %>%
    filter(HomeTeam == teamName | AwayTeam == teamName) %>%
    select(Date) # %>% head(10)

  
  numEvents = length(gameDates[,1]) - N + 1
  
  x_tn <- rep(teamName, numEvents)
  x_st <- gameDates[1:numEvents, 1]
  x_en <- gameDates[(N+1):(length(gameDates[,1])), 1]
  x_en <- c(x_en, x_en[length(x_en)] + 1)

  df <- data.frame( team = x_tn,
                    startDate = x_st,
                    endDate = x_en)

  bind_rows(apply(df, 1, print_performance, data))
}

x <- nGamesTeamPerformance(8, "Liverpool")

plot(x$Pts)
