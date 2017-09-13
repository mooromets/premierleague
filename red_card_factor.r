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

ggplot(data = rate, aes(oddsDiffCat, group = 1)) + 
  geom_line(aes(y = lost.no_Adv, colour = "lost.no_Adv")) +
  geom_line(aes(y = won.no_Adv, colour = "won.no_Adv"))

library("reshape2")
library("ggplot2")

test_data_long <- melt(rate, id="oddsDiffCat")  # convert to long format

ggplot(data=test_data_long,
       aes(x=oddsDiffCat, group =1, y=value, colour=variable)) +
  geom_line()



ggplot(data = rate, aes(oddsDiffCat, group = 1)) + 
  geom_line(aes(y = lost.no_Adv, colour = "lost.no_Adv")) +
  geom_line(aes(y = won.no_Adv, colour = "won.no_Adv")) +
  geom_line(aes(y = drawn.no_Adv, colour = "drawn.no_Adv")) +
  geom_line(aes(y = lost.Adv, colour = "lost.Adv")) +
  geom_line(aes(y = won.Adv, colour = "won.Adv")) +
  geom_line(aes(y = drawn.Adv, colour = "drawn.Adv"))

