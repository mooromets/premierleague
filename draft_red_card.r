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

## Density plot by W-D-L
ggplot(aes(oddsDiff, fill = advantage), data = cards) +
  geom_density(alpha=0.25) + 
  facet_grid(. ~ FTR)

## Win-percenage curves
degree <- 5
ggplot(data = rate, aes(oddsDiffCat, group = 1)) + 
#  geom_line(aes(y = lost.no_Adv), colour = "#FFAAAA") +
#  stat_smooth(aes(y=lost.no_Adv), colour = "#8B0000",  method = lm, formula = y ~ poly(x, degree), se = FALSE) +
#  geom_line(aes(y = lost.Adv), colour = "#FFAAAA") +
#  stat_smooth(aes(y=lost.Adv), colour = "#8B0000",  method = lm, formula = y ~ poly(x, degree), se = FALSE) +
  
  
  geom_line(aes(y = won.no_Adv), colour = "#AAFFAA" ) +
  stat_smooth(aes(y = won.no_Adv, colour = "won.no_Adv"),  method = lm, formula = y ~ poly(x, degree), se = FALSE) +
  geom_line(aes(y = drawn.no_Adv), colour = "#AAAAFF" ) +
  stat_smooth(aes(y = drawn.no_Adv, colour = "drawn.no_Adv"),  method = lm, formula = y ~ poly(x, degree), se = FALSE) +

  geom_line(aes(y = won.Adv), colour = "#FFFFAA" ) +
  stat_smooth(aes(y = won.Adv, colour = "won.Adv"),  method = lm, formula = y ~ poly(x, degree), se = FALSE) +
  geom_line(aes(y = drawn.Adv), colour = "#FFAAFF" ) +
  stat_smooth(aes(y = drawn.Adv, colour = "drawn.Adv"),  method = lm, formula = y ~ poly(x, degree), se = FALSE) +
  
    
      
#  geom_line(aes(y = drawn.no_Adv, colour = "drawn.no_Adv")) +
#  geom_line(aes(y = lost.Adv, colour = "lost.Adv")) +
#  geom_line(aes(y = won.Adv, colour = "won.Adv")) +
#  geom_line(aes(y = drawn.Adv, colour = "drawn.Adv")) +
  scale_color_manual(values = c(won.no_Adv = "#008B00", drawn.no_Adv = "#00008B", 
                                won.Adv = "#708B00", drawn.Adv = "#70008B")) +
  #scale_x_continuous(limits = c(-12, 12))
  coord_cartesian(xlim = c(-12, 13)) 

ggplot(aes(oddsDiff), data = cards) +
  geom_density(alpha=0.25) + coord_cartesian(xlim = c(-12, 13))


genPlot <- ggplot(data = cards, aes(oddsDiffCat)) +  
(dens <- genPlot + geom_density(alpha=0.25))
