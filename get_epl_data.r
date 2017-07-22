# download and/or read English Premier League data

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

# read seasons' data from the specified files
# returns a data frame 
readSeasons <- function(fileslist) {
  src <- "http://www.football-data.co.uk/mmz4281/"
  for (f in fileslist) {
    if (!file.exists(f))
      download.file(url = paste(src, gsub("-", "/", f), sep = ""),
                    destfile = f)
  }
  colclasses = c(rep("factor", 4), 
                   rep("integer", 2),
                   "factor",
                   rep("integer", 2),
                   rep("factor", 2),
                   rep("integer", 12),
                   rep("NULL", 50))
  do.call(rbind, lapply(fileslist, read.csv, colClasses = colclasses))
}
