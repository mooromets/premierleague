# clean and reshape raw data

library(dplyr)
library(lubridate)

selectSide <- function (side, data) {
  if (side == 'H') {
    thisTeam <- "HomeTeam"
    oppTeam <- "AwayTeam"
    oddsAvgThis <- "BbAvH"
    oddsAvgOpp <- "BbAvA"
    teamRedC <- "HR"
    oppRedC <- "AR"
  } else if (side == 'A') {
    thisTeam <- "AwayTeam"
    oppTeam <- "HomeTeam"      
    oddsAvgThis <- "BbAvA"
    oddsAvgOpp <- "BbAvH"
    teamRedC <- "AR"
    oppRedC <- "HR"
  }
  select(data, everything()) %>%
    mutate(field = side,
           FTR = ifelse(FTR == side, 'W', ifelse(FTR == 'D', 'D', 'L')),
           Pts = ifelse(FTR == 'W', 3, ifelse(FTR == 'D', 1, 0)) ) %>%
    rename_(team = thisTeam, opp = oppTeam,
            teamOddsWinAv = oddsAvgThis, teamOddsOppAv = oddsAvgOpp,
            teamRC = teamRedC, oppRC = oppRedC)    
}

basicClean <- function(data) {
  data <- mutate(data,
                 Date = dmy(Date), # character to 
                 # year - year of the season's end
                 year = ifelse(month(Date) > 7, year(Date)+1, year(Date)))
  #add odds variable that played through
  nColFTR <- grep("^FTR$", colnames(data))
  data$odds.played <- apply(data, 1, FUN = 
                      function(x) as.numeric( x[ paste0("B365", x[nColFTR]) ] ))
  # split every obs into two while merging columns HomeTeam and AwayTeam into one
  rbind (selectSide('H', data), selectSide('A', data))
}