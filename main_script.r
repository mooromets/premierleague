source("./get_epl_data.r")
source("./newcomers_performance.r")
source("./reshape_data.r")


downloadSeasons(2001:2017,1:2)

data <- readSeasonsData(2001:2017, c(1,2), c("Div", "Date", "HomeTeam", "AwayTeam", "FTR"))

data <- basicDataClean(data)

stats <- do.call("rbind", lapply (2002:2017, getNewcomersStats, data))
stats$Div2.place <- as.factor(stats$Div2.place)
stats$Div1.year <- as.factor(stats$Div1.year)

plot(stats$Pts.Div2, stats$Pts.Div1)

plot(stats$Div2.place, stats$Pts.Div1)

plot(stats$Div1.year, stats$Pts.Div1)

library(ggplot2)

g <- ggplot(data = stats, aes(x = Div1.year, y = Pts.Div1, group = Div2.place))
g + geom_line(aes(color = Div2.place))