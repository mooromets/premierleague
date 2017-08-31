source("./get_epl_data.r")
source("./reshape_data.r")
source("./metrics.r")

library(ggplot2)

divEPL <- "E0"
form_games_num <- 5

# sampling
set.seed(278)
seasons <- 2005:2017
train_set <- sample(seasons, 7)
test_set <- sample(setdiff(seasons, train_set), 3)
validation_set <- setdiff(seasons, union(train_set, test_set))

# getting data
downloadSeasons(2003:2017,1:2)
data <- readSeasonsData(2003:2017, c(1,2), c("Div", "Date", "HomeTeam", "AwayTeam", "FTR"))
data <- basicDataClean(data)

train_data  <- data %>%
  #TODO how to do meta programming in R?
  filter(Div == divEPL & year == train_set[1] | year == train_set[2] | 
           year == train_set[3] | year == train_set[4] | year == train_set[5] | 
           year == train_set[6] | year == train_set[7])

out <- data.frame ()

all_matchdays <- unique(train_data[train_data$Div == divEPL, "Date"])
#all_matchdays <- c(ymd("2004-11-06"))
for (matchday in all_matchdays) {
  #progress 
  print(round(match(matchday, all_matchdays) / length(all_matchdays), 4))
  
  matchday <- as.Date(matchday, origin = "1970-01-01")

  games <- data %>% filter(Div == divEPL, Date == matchday, field == 'H')
  games$team <- as.character(games$team)
  games$opp <- as.character(games$opp)

  # !!! using (full) data for calculation features, not train data
  relPPG <-  
    points_sprint_relative_speed(data, 
                                     form_games_num, 
                                     divEPL,
                                     as.Date(matchday, origin = "1970-01-01"), 
                                     c(games$team, games$opp), 
                                     rulesEPL)

  games <- left_join(games, relPPG, by = c("team", "field"))
  
  games <- left_join(games, 
            relPPG[relPPG$field == 'A',], #opposition always away 
            by = c("opp" = "team"),
            suffix = c(".team", ".opp"))
  
  out <- rbind(out, games[,c("Date", "year", "team", "ppg_rel.team", "opp", "ppg_rel.opp", "FTR")])
}

clean <- out[complete.cases(out),]
clean <- clean[clean$ppg_rel.team != 0, ] #dunno what to do with zeros at the moment
clean <- clean[clean$ppg_rel.opp != 0, ]
clean$k <- clean$ppg_rel.team/clean$ppg_rel.opp

qplot(clean$k, colour = clean$FTR)
ggplot(clean, aes(k)) + geom_histogram()+facet_grid(clean$FTR ~ .)
qplot(clean$ppg_rel.team, clean$ppg_rel.opp, colour = clean$FTR, alpha=I(0.5))

p <- ggplot(clean, aes(x = ppg_rel.team, y = ppg_rel.opp)) + geom_point() 
p + facet_grid(. ~ FTR)

ggplot() +
geom_density(aes(ppg_rel.team), data = clean, colour = "yellow") +
geom_density(aes(ppg_rel.opp), data = clean, colour = "orange") +
facet_grid(. ~ FTR)
