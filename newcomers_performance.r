source("./reshape_data.r")

intersect(unique(filter(data, Div == "E0", year == 17) %>% select (HomeTeam))$HomeTeam,
          unique(filter(data, Div == "E1", year == 16) %>% select (HomeTeam))$HomeTeam)

intersect(unique(filter(data, Div == "E0", year == 16) %>% select (HomeTeam))$HomeTeam,
          unique(filter(data, Div == "E1", year == 15) %>% select (HomeTeam))$HomeTeam)