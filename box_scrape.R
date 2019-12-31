library(tidyverse)
library(rvest)
#######################################
get_box <- function(link){
  #read the html link
  ff <- read_html(link)
  
#get the column names
alltext <- ff %>% html_nodes('tr') %>% html_children() %>%  html_text()

starts <-which(alltext=='Starters')
res <- which(alltext=='Reserves')
sts <- which(alltext=='School Totals')
pts <- which(alltext=='PTS')
bbs <- which(alltext=='Basic Box Score Stats')

cnm <- c('Name',alltext[(bbs[1]+2):(bbs[1]+23)])

#get the teams
ids <- ff %>% html_nodes('div > h1') %>% html_text() %>%
  str_sub(end=str_locate(.,' Box Score, ')[1]-1) %>% str_split(' vs. ') %>% unlist()

dt<- ff %>% html_nodes('div > h1') %>% html_text() %>% str_sub(start=str_locate(.,' Box Score, ')[2]+1) %>%
  lubridate::mdy()

print(paste('Now reading:', ids[1], 'vs.', ids[2], dt))


#find the away team
awayatts <- alltext[c((pts[1]+1):(sts[1]-1))]
awayatts <- awayatts[which(!(awayatts %in% c('Starters', 'Reserves',cnm)))] %>%
  matrix(ncol=length(cnm), byrow=TRUE)  %>%
  as_tibble()

awayatts <- suppressMessages(type_convert(awayatts))

#find the home team
homeatts <- alltext[c((bbs[2]+24):(sts[2]-1))]
homeatts <- homeatts[which(!(homeatts %in% c('Starters', 'Reserves',cnm)))] %>%
  matrix(ncol=length(cnm), byrow=TRUE)  %>%
  as_tibble()
homeatts <- suppressMessages(type_convert(homeatts))

#name the columns and put the boxscores in a list
colnames(homeatts) <- colnames(awayatts) <- cnm
boxed <- list(meta=c(home=ids[2], homescore=sum(homeatts$PTS), away=ids[1],
                     awayscore=sum(awayatts$PTS),date=as.character(dt)),
              home=homeatts, away=awayatts)

Sys.sleep(2)
return(boxed)
}

