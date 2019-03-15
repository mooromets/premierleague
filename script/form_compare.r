source("./lib/download.R")
source("./lib/read_data.R")
source("./lib/table.R")

#################

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
                   endDate = c("2018-10-30", "2018-12-31", "2019-03-15"))

x <- bind_rows(apply(pool, 1, print_performance, data))

city <- data.frame(team = c("Man City", "Man City", "Man City"),
                   startDate = c("2018-08-10", "2018-10-30", "2018-12-31"),
                   endDate = c("2018-10-30", "2018-12-31", "2019-03-15"))

xCity <- bind_rows(x, apply(city, 1, print_performance, data))



##################################

x <- nGamesTeamPerformance(9, "Liverpool")
cy <- nGamesTeamPerformance(9, "Man City")

plot(x$game, x$Pts, type = 'l', col = "red", ylim = c(14, 27))
lines(cy$game, cy$Pts, type = 'l', col = "blue")
#abline(h=17.76, col="blue")

invers <- data.frame(game = x$game, Pts = 27 -x$Pts + 14)
plot(cy$game, cy$Pts, type = 'l', col = "blue", ylim = c(14, 27))
lines(invers$game, invers$Pts, type = 'l', col = "green")



