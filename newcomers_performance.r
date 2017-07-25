#source("./reshape_data.r")

# calculate points for the specified teams in between fromDate and toDate
points <- function (data, fromDate, toDate, div, teams) {
  data %>%
    #filter by input params
    filter(Div == div, between(Date, fromDate, toDate), team %in% teams) %>%
    group_by(team) %>%
    summarise(Pts = sum(Pts)) %>%
    arrange(-Pts)
}

#points(data, ymd("04-07-01"), ymd("04-09-01"), "E1", c("Ipswich", "Coventry", "Brighton", "Leeds"))

getNewcomersStats <- function(y) {
  inter <- intersect(unique(filter(data, Div == "E0", year == y) %>% select (team))$team,
                     unique(filter(data, Div == "E1", year == y-1) %>% select (team))$team)
  stats <- full_join(
    as.data.frame(
      points(data, ymd((y-2)*10000 + 0701), ymd((y-1)*10000 + 0701), "E1", inter)),
    as.data.frame(
      points(data, ymd((y-1)*10000 + 0701), ymd((y)*10000 + 0701), "E0", inter)),
    by = c("team"),
    suffix = c(".Div2", ".Div1")
  )
  stats["Div2.place"] <- c(1,2,3)
  stats["Div1.year"] <- rep(y, 3)
  stats
}

stats <- do.call("rbind", lapply (2006:2017, getNewcomersStats))

plot(stats$Pts.Div2, stats$Pts.Div1)

plot(stats$Div2.place, stats$Pts.Div1)

plot(stats$Div1.year, stats$Pts.Div1)

library(ggplot2)

g <- ggplot(data = stats, aes(x = Div1.year, y = Pts.Div1, group = Div2.place))
g + geom_line(aes(color = Div2.place))

