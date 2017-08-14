# clean and reshape raw data

library(dplyr)
library(lubridate)

basicDataClean <- function(data) {
  # convert 'Date' variable to a Date format and add 'year' variable 
  data <- mutate(data, Date = dmy(Date), year = ifelse(month(Date) > 7, 
                                                       year(Date)+1, year(Date)))
  
  # make two columns HomeTeam and AwayTeam into single one
  rbind (
    # Home
    select(data, c("Div", "Date", "year", "HomeTeam", "FTR", "AwayTeam")) %>%
      mutate(field = "H", 
             FTR = ifelse(FTR == 'H', 'W', ifelse(FTR == 'D', 'D', 'L')),
             Pts = ifelse(FTR == 'W', 3, ifelse(FTR == 'D', 1, 0)) ) %>%
      rename (team = HomeTeam, opp = AwayTeam),
    # Away
    select(data, c("Div", "Date", "year", "AwayTeam", "FTR", "HomeTeam")) %>%
      mutate(field = "A", 
             FTR = ifelse(FTR == 'A', 'W', ifelse(FTR == 'D', 'D', 'L')),
             Pts = ifelse(FTR == 'W', 3, ifelse(FTR == 'D', 1, 0)) ) %>%
      rename (team = AwayTeam, opp = HomeTeam)
  )
}