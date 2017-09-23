source("../get_epl_data.r")
source("../reshape_data.r")
source("../metrics.r")

divEPL <- "E0"
form_games_num <- 5

# getting data
setwd("../data")
downloadSeasons(2003:2017,1:2)
data <- readSeasonsData(2003:2017, c(1,2))
setwd("../tests")

data <- basicClean(data)
teams <-  c("Liverpool", "Arsenal", "Chelsea", "Everton")
check <- data.frame(team = c("Arsenal", "Liverpool", "Arsenal", "Liverpool", 
                              "Chelsea", "Chelsea", "Everton","Everton"),
                    field = c("H", "H", "A", "A", "A", "H", "A", "H"),
                    Pts = c(40, 32, 31, 28, 26, 24, 24, 23),
                    games = rep(19, 8),
                    stringsAsFactors = FALSE)

res <- points_cross (data, teams, distance1516) 

test_that("variables have the same names", {
  expect_equal(attr(res, "names"), attr(check, "names"))
})


test_that("data arranged in the same order", {
  expect_equivalent(check, res)
})