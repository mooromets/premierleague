source("./lib/download.R")
source("./lib/read_data.R")
source("./lib/table.R")

#################

downloadSeasons(2018:2018,1)
data <- readSeasonsData(2018:2018,1)

# only needed columns 
data <- data[ , c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]

# convert string date
data <- mutate(data,
               Date = dmy(Date) # character to 
               # year - year of the season's end 
               #, year = ifelse(month(Date) > 7, year(Date)+1, year(Date))
)

lt <- league_table(data, side = 'All')
teams <- unique(c(as.character(data$HomeTeam), as.character(data$AwayTeam)))
nGames <- c(5:8)
resN <- vector(mode = "integer")
resR <- vector(mode = "numeric")

#TODO remove for with an apply function
for(te in teams)
{
  #calculate avg difficulty
  avg <- mean(lt[lt$team != te, "avg.Points"])
  print(sprintf("%s %f", te, avg))
  
  resN <- append(resN, 2)
  resR <- append(resR, avg)
}

