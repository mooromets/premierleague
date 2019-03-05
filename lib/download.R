# download CSV files with english football data from football-data.co.uk

library(dplyr)

linkFDCoUk <- "http://www.football-data.co.uk/mmz4281/"

# @brief download data from internet
# @param seasons - a range of seasons 2005:2007, where the season 2005-06
# is meant '2006'
# @divisions - a set, defining English divisions:
#               1 for Premier League
#               2 for Championship,
#               3 and 4 for League 1 and League 2 respectively
#
# @output every season file will be named yy-YY-EX
#           yy - start year
#           YY - end year (== yy+1)
#           X - division number
#
# @example  downloadSeasons(2017:2017,1) - download single EPL season 2016-17 
downloadSeasons <- function (seasons, divisions) {
  files <- seasonFiles(seasons, divisions)
  for (f in files)
    if (!file.exists(f)) download.file(url = paste(linkFDCoUk, gsub("-", "/", f), 
                                                   sep = ""), destfile = f)
}



# get the list of files assigned to the selected seasons and divisions
# returns a list
seasonFiles <- function(seasons, divisions) {
  eplDivisions = list(div1 = "E0", div2 = "E1", div3 = "E2", div4 = "E3")
  l = character(0)
  for (s in seasons)
    for (d in divisions) {
      year <- paste(sprintf("%02d", s-1 - 2000), sprintf("%02d", s - 2000), sep = "")
      filename <- paste(year, "-", eplDivisions[[d]], ".csv", sep = "")
      l <- c(l, filename)
    }
  l
}