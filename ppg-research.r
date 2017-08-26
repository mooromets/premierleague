source("./get_epl_data.r")
source("./reshape_data.r")
source("./metrics.r")

divEPL <- "E0"
form_games_num <- 5

# sampling
set.seed(278)
seasons <- 2005:2017
train_set <- sample(seasons, 7)
test_set <- sample(setdiff(seasons, train), 3)
validation_set <- setdiff(seasons, union(train, test))

# getting data
downloadSeasons(2005:2017,1:2)
data <- readSeasonsData(2005:2017, c(1,2), c("Div", "Date", "HomeTeam", "AwayTeam", "FTR"))
data <- basicDataClean(data)

train_data  <- data %>%
  #TODO how to do meta programming in R?
  filter(Div == divEPL & year == train_set[1] | year == train_set[2] | 
           year == train_set[3] | year == train_set[4] | year == train_set[5] | 
           year == train_set[6] | year == train_set[7])

points_sprint_relative_speed(train_data, form_games_num, divEPL, ymd("2015-08-01"), 
                             c("Liverpool", "Chelsea", "Arsenal", "Everton"), 
                             rulesEPL)