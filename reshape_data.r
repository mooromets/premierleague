# clean and reshape raw data

library(dplyr)
library(lubridate)

source("./get_epl_data.r")

data <- readSeasons(seasonFiles(c(2017, 2016, 2010), c (1,2)))

# convert 'Date' variable to a Date format and add 'year' variable 
data <- mutate(data, Date = as.Date(as.character(Date), "%d/%m/%Y"),
            year = ifelse(month(Date) > 7, year(Date)+1, year(Date)))