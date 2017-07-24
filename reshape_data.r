# clean and reshape raw data

library(dplyr)
library(lubridate)

source("./get_epl_data.r")

#data <- d <- readSeasonsData(2012:2017,c(1,2), c("Div", "Date", "HomeTeam", "AwayTeam", "FTR"))

# convert 'Date' variable to a Date format and add 'year' variable 
data <- mutate(data, Date = as.Date(as.character(Date), "%d/%m/%Y"),
            year = ifelse(month(Date) > 7, year(Date)+1, year(Date)))

# add points
data <- mutate(data, HoPts = ifelse(FTR == 'H', 3, ifelse(FTR == 'D', 1, 0)),
                    AwPts = ifelse(FTR == 'A', 3, ifelse(FTR == 'D', 1, 0)))