source("./reshape_data.r")

d <- data.frame()
mylist <- list()
for (y in 6:17) {
  #find newcomers in year y
  mylist[length(mylist)+1] <- list(
                    intersect(unique(filter(data, Div == "E0", year == y) %>% select (HomeTeam))$HomeTeam,
                           unique(filter(data, Div == "E1", year == y-1) %>% select (HomeTeam))$HomeTeam)
                 )
  
}
