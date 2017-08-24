#source("./reshape_data.r")

#TODO : use metrics.r
# calculate points for the specified teams in between fromDate and toDate
points <- function (data, fromDate, toDate, div, teams) {
  data %>%
    #filter by input params
    filter(Div == div, between(Date, fromDate, toDate), team %in% teams) %>%
    group_by(team, field) %>%
    summarise(Pts = sum(Pts), games = n()) %>%
    arrange(-Pts)
}

#points(data, ymd("04-07-01"), ymd("04-09-01"), "E1", c("Ipswich", "Coventry", "Brighton", "Leeds"))

getNewcomersStats <- function(y, data) {
  inter <- intersect(unique(filter(data, Div == "E0", year == y) %>% select (team))$team,
                     unique(filter(data, Div == "E1", year == y-1) %>% select (team))$team)
  stats <- full_join(
    as.data.frame(
      points(data, ymd((y-2)*10000 + 0701), ymd((y-1)*10000 + 0701), "E1", inter)),
    as.data.frame(
      points(data, ymd((y-1)*10000 + 0701), ymd((y)*10000 + 0701), "E0", inter)),
    by = c("team", "field"),
    suffix = c(".Div2", ".Div1")
  )
  stats["Div2.place"] <- c(1,2,3)
  stats["Div1.year"] <- rep(y, 3)
  stats
}