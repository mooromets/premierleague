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

teams1516 <-  c("Liverpool", "Arsenal", "Chelsea", "Everton")
check1516 <- data.frame(team = c("Arsenal", "Liverpool", "Arsenal", "Liverpool", 
                              "Chelsea", "Chelsea", "Everton","Everton"),
                    field = c("H", "H", "A", "A", "A", "H", "A", "H"),
                    Pts = c(40, 32, 31, 28, 26, 24, 24, 23),
                    games = rep(19, 8),
                    stringsAsFactors = FALSE)
res1516 <- points_cross (data, teams1516, distance1516) 

distance1415 <- list(
  div = "E0"
  ,period =  c(ymd("20140801"), ymd("20150801"))
)
teams1415 <-  c("Man City", "West Ham", "QPR", "West Brom")
check1415 <- data.frame(team = c("Man City", "Man City", "West Ham", "West Brom", 
                                 "QPR", "West Brom", "West Ham", "QPR"),
                        field = c("H", "A", "H", "H", "H", "A", "A", "A"),
                        Pts = c(45, 34, 31, 25, 23, 19, 16, 7),
                        games = rep(19, 8),
                        stringsAsFactors = FALSE)
res1415 <- points_cross (data, teams1415, distance1415) 

test_that("variables have the same names", {
  expect_equal(attr(res1516, "names"), attr(check1516, "names"))
  expect_equal(attr(res1415, "names"), attr(check1415, "names"))
})


test_that("data arranged in the same order", {
  expect_equivalent(check1516, res1516)
  expect_equivalent(check1415, res1415)
})