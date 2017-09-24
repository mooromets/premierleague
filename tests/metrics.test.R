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

test_that("points_cross variables have the same names", {
  expect_equal(attr(res1516, "names"), attr(check1516, "names"))
  expect_equal(attr(res1415, "names"), attr(check1415, "names"))
})

test_that("points_cross data arranged in the same order", {
  expect_equivalent(check1516, res1516)
  expect_equivalent(check1415, res1415)
})

res_ppg_1415 <- points_cross_speed(data, teams1415, distance1415, rulesEPL)
check_ppg_1415 <- data.frame(team = c("Man City", "West Brom", "West Ham", "QPR",
                                      "Man City", "West Ham", "West Brom", "QPR"),
                        field = c(rep("A", 4), rep("H", 4)),
                        PPG = c(1.7894737, 1.0000000, 0.8421053, 0.3684211, 2.3684211, 
                                1.6315789, 1.3157895, 1.2105263),        
                        games = rep(19, 8),
                        Pts = c(34, 19, 16, 7, 45, 31, 25, 23),
                        stringsAsFactors = FALSE)

res_ppg_1516 <- points_cross_speed(data, teams1516, distance1516, rulesEPL)
check_ppg_1516 <- data.frame(team = c("Arsenal", "Liverpool", "Chelsea", "Everton", 
                                      "Arsenal", "Liverpool", "Chelsea", "Everton"),
                             field = c(rep("A", 4), rep("H", 4)),
                             PPG = c(1.631579, 1.473684, 1.368421, 1.263158, 2.105263, 
                                     1.684211, 1.263158, 1.210526),        
                             games = rep(19, 8),
                             Pts = c(31, 28, 26, 24, 40, 32, 24, 23),
                             stringsAsFactors = FALSE)

test_that("PPG in points_cross_speed are correct", {
  expect_equal(res_ppg_1415$PPG, check_ppg_1415$PPG, tolerance = .001)
  expect_equal(res_ppg_1516$PPG, check_ppg_1516$PPG, tolerance = .001)
})

test_that("points_cross_speed has the same variables", {
  expect_equal(attr(res_ppg_1415, "names"), attr(check_ppg_1415, "names"))
  expect_equal(attr(res_ppg_1516, "names"), attr(check_ppg_1516, "names"))
})

test_that("points_cross_speed returned data.frame are equal", {
  expect_equal(res_ppg_1415, check_ppg_1415, tolerance = .001)
  expect_equal(res_ppg_1516, check_ppg_1516, tolerance = .001)
})
