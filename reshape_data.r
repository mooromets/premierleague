# clean and reshape raw data

library(dplyr)
library(lubridate)

basicDataClean <- function(data) {
  # convert 'Date' variable to a Date format and add 'year' variable 
  data <- dplyr::mutate(data, Date = dmy(Date), year = ifelse(month(Date) > 7, 
                                                       year(Date)+1, year(Date)))
  
  selectBySide <- function (side) {
    if (side == 'H') {
      thisTeam <- "HomeTeam"
      oppTeam <- "AwayTeam"
    } else if (side == 'A') {
      thisTeam <- "AwayTeam"
      oppTeam <- "HomeTeam"      
    }
    select(data, c("Div", "Date", "year", "HomeTeam", "FTR", "AwayTeam")) %>%
      mutate(field = side, 
             FTR = ifelse(FTR == side, 'W', ifelse(FTR == 'D', 'D', 'L')),
             Pts = ifelse(FTR == 'W', 3, ifelse(FTR == 'D', 1, 0)) ) %>%
      rename_(team = thisTeam, opp = oppTeam)    
  }
  
  # make two columns HomeTeam and AwayTeam into single one
  rbind (  selectBySide('H'), selectBySide('A'))
}