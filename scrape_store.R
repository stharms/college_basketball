library(tidyverse)
library(lubridate)
library(rvest)
library(DBI)
library(RSQLite)
library(dbplyr)


source('get_links.R')
source('prep_boxes.R')
source('box_scrape.R')


#build set of dates
dates <- seq.Date(from=mdy('11-06-2018'), to=mdy('03-17-2019'), by='day') %>% ymd()
#read each date, then get all of the links
#read all of the links and extract box scores, then get them ready
firsthalf <- dates %>% map(build_href) %>% map(get_links)  %>% unlist() %>%  map(get_box) %>% prep_boxes

mydb <- dbConnect(RSQLite::SQLite(), "ncaa2018-19.sqlite")
dbWriteTable(mydb, 'games', firsthalf$gameinfo)
dbWriteTable(mydb, 'homebox', firsthalf$home)
dbWriteTable(mydb, "awaybox", firsthalf$away)

#test it!
dbGetQuery(mydb, 'SELECT * FROM homebox LIMIT 20')

dbDisconnect(mydb)



