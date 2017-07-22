source("./reshape_data.r")

unique(
select(data, HomeTeam, year, Div) %>%
  filter(Div == "E0", year == 16) %>%
  select (HomeTeam) )