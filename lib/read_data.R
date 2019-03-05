
source("./lib/download.R") 

# @brief read English football data from a local files
# @param seasons - a range of seasons 2005:2007, where the season 2005-06
# is meant '2006'
# @divisions - a set, defining English divisions:
#               1 for Premier League
#               2 for Championship,
#               3 and 4 for League 1 and League 2 respectively
#
# @output returns a data.frame with the data
#
# @example  downloadSeasons(2017:2017,1) - read single EPL season 2016-17 
readSeasonsData <- function(seasons, divisions) {
  files <- seasonFiles(seasons, divisions)
  do.call(bind_rows, lapply(files,  read.csv))
}
