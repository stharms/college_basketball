library(tidyverse)
library(rvest)
library(lubridate)
###########################
build_href <- function(gameday, format='ymd'){
  if(format=='mdy') {gameday <- mdy(gameday)}
  gameday <- ymd(gameday)
  mo <- month(gameday)
  yr <- year(gameday)
  dy <- day(gameday)
  outref <- paste0('https://www.sports-reference.com/cbb/boxscores/index.cgi?month=',
                   mo,'&day=',dy,'&year=',yr)
  return(outref)
}

#################################3
get_links <- function(href) {
ff <- read_html(href)
links <- ff %>% html_nodes('div') %>% html_nodes('tr') %>% html_nodes('a') %>% html_attr('href') 
links <- links[str_which(links,pattern='boxscore')]
if(length(links)==0) return(NULL)
links <- paste0('https://www.sports-reference.com/', links)
return(links)
}


'2019-11-16' %>% ymd() %>% build_href %>% get_links %>% map(get_box) %>% prep_boxes
