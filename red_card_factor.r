source("../get_epl_data.r")
source("../reshape_data.r")
library(ggplot2)

setwd("../data")
downloadSeasons(2007:2017,1)
data <- readSeasonsData(2007:2017, 1)
setwd("../reports")

data <- basicClean(data)
# subset data
cards <- data %>% 
  select (FTR, teamRC, oppRC, teamOddsWinAv, teamOddsOppAv)

# introduce advantage and oddsDiff variables
# oddsDiff - advantage in the odds for the opponent
cards <- cards %>% 
  mutate(oddsDiff = teamOddsOppAv - teamOddsWinAv,
         advantage = ifelse(teamRC == oppRC, FALSE, TRUE)) %>%
  filter(!is.na(oddsDiff))

ggplot(aes(oddsDiff, fill = advantage), data = cards)+
  geom_density(alpha=0.25) + 
  facet_grid(. ~ FTR)

cards %>% 
  select(oddsDiff, advantage, FTR) %>%
  group_by(FTR, advantage) %>%
  summarise(oddsAvg = mean(oddsDiff), oddsMed = median(oddsDiff))


