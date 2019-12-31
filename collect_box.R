library(tidyverse)
library(DBI)
#####################################################
#given a box score or season totals, compute advanced stats
collect_box <-
  function(hometeam = NULL,
           awayteam = NULL,
           dt = NULL,
           game_id = NULL) {
    #find the game in the game database
    if (is.null(game_id)) {
      game <- games %>% filter(home == hometeam) %>%
        filter(away == awayteam)  %>%
        filter(date == dt) %>% collect()
    }
    if (!is.null(game_id)) {
      game <- games %>% filter(gameid == game_id) %>% collect()
    }
    #collect home team box
    homebox <-
      homes %>% filter(gameid == (game$gameid)) %>% collect() %>% data.frame()
    hometotals <-
      c(
        game$gameid[1],
        homebox$team[1],
        'Totals',
        colSums(homebox[, 4:6]),
        round(sum(homebox$FG) / sum(homebox$FGA), 4),
        colSums(homebox[, 8:9]),
        round(sum(homebox$`X2P`) / sum(homebox$`X2PA`), 4),
        colSums(homebox[, 11:12]),
        round(sum(homebox$`X3P`) / sum(homebox$`X3PA`), 4),
        colSums(homebox[, 14:15]),
        round(sum(homebox$FT) / sum(homebox$FTA), 4),
        colSums(homebox[, 17:25])
      )
    homebox <- rbind(homebox, hometotals)
    #collect away team box
    awaybox <-
      aways %>% filter(gameid == game$gameid) %>% collect() %>% data.frame()
    awaytotals <-
      c(
        game$gameid[1],
        awaybox$team[1],
        'Totals',
        colSums(awaybox[, 4:6]),
        round(sum(awaybox$FG) / sum(awaybox$FGA), 4),
        colSums(awaybox[, 8:9]),
        round(sum(awaybox$`X2P`) / sum(awaybox$`X2PA`), 4),
        colSums(awaybox[, 11:12]),
        round(sum(awaybox$`X3P`) / sum(awaybox$`X3PA`), 4),
        colSums(awaybox[, 14:15]),
        round(sum(awaybox$FT) / sum(awaybox$FTA), 4),
        colSums(awaybox[, 17:25])
      )
    awaybox <- rbind(awaybox, awaytotals)
    #list and output
    return(list(
      gamedata = game,
      homebox = homebox,
      awaybox = awaybox
    ))
    
  }

#####################################################
#####################################################
#collect all games for a team
collect_team <- function(tm = NULL) {
  if (!is.null(tm)) {
    ab <- homes %>% filter(team == tm) %>%
      select(gid = gameid) %>% collect() %>%  unique() %>%
      union_all((
        aways %>% filter(team == tm) %>%
          select(gid = gameid) %>% collect() %>%  unique()
      ))
  }
  gamelist <- ab$gid %>% map(function(x)
    collect_box(game_id = x))
  gamedf <- gamelist %>% pluck('homebox') %>% bind_rows %>%
    union_all((gamelist %>% pluck('awaybox') %>% bind_rows)) %>% left_join((gamelist %>% pluck('gamedata') %>% bind_rows))
  
  gamelist <-
    list(boxes = (gamedf %>% filter(Name != 'Totals')),
         Totals = (gamedf %>% filter(Name == 'Totals')))
  return(gamelist)
}
#####################################################

#####################################################
collect_player <- function(plyr, tm) {
  abs <- collect_team(tm)
  filt <-
    abs$boxes %>% filter(Name == plyr) %>% select(-Name) %>% type_convert %>%
    left_join((
      abs$Totals %>% filter(team != tm) %>%
        mutate(opponent = team) %>% select(gameid, opponent) %>% type_convert
    ),
    by = c('gameid'))
  
  totals <-
    filt %>% select(-gameid)  %>% mutate(GP = 1) %>%  group_by(team) %>%
    summarise_if(is.numeric, sum) %>% mutate(
      FG. = FG / FGA,
      X2P. = X2P / X2PA,
      X3P. = X3P / X3PA,
      FT. = FT / FTA
    )
  averages <-
    totals %>% mutate_if(is.numeric, function(x)
      x / nrow(filt)) %>%
    mutate(
      FG. = round(FG / FGA, 2),
      X2P. = round(X2P / X2PA, 2),
      X3P. = round(X3P / X3PA, 2),
      FT. = round(FT / FTA, 2)
    )
  return(list(
    games = filt,
    totals = totals,
    averages = averages
  ))
}
#####################################################

#test it out down here
#connect to the dbs first
homes <- tbl(mydb, 'homebox')
aways <- tbl(mydb, 'awaybox')
games <- tbl(mydb, 'games')


collect_box('Kansas', 'Texas', dt = '2019-01-14')
collect_box(game_id = '201811063136')
collect_team('Kansas')$Totals
collect_player('Zion Williamson', 'Duke')
