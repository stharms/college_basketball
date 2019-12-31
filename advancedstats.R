library(tidyverse)
library(DBI)

setwd('~/Documents/R/CBB') #change this to your previous directory

source('collect_box.R')

#given a complete box score, compute all of the advanced statistics
#at both the team level and player level
advanced_box_stats <- function(collectedbox) {
  homestats <-
    collectedbox$homebox %>% filter(Name != 'Totals') %>% type_convert()
  hometots <-
    collectedbox$homebox %>% filter(Name == 'Totals') %>% type_convert()
  awaystats <-
    collectedbox$awaybox %>% filter(Name != 'Totals') %>% type_convert()
  awaytots <-
    collectedbox$awaybox %>% filter(Name == 'Totals') %>% type_convert()
  pos <-
    round(
      0.5 * (
        hometots$TOV + hometots$FGA - hometots$ORB + .44 * hometots$FTA +
          awaytots$TOV + awaytots$FGA - awaytots$ORB +
          .44 * awaytots$FTA
      ),
      0
    )
  homepos <-
    round((hometots$TOV + hometots$FGA - hometots$ORB + .44 * hometots$FTA),
          0)
  awaypos <-
    round((awaytots$TOV + awaytots$FGA - awaytots$ORB + .44 * awaytots$FTA),
          0)
  
  h_imdt <- homestats %>% mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      qAST = ((MP / (hometots$MP / 5)) *
                (1.14 * ((hometots$AST - AST) / hometots$FG
                ))) +
        ((((hometots$AST / hometots$MP) * MP *
             5 - AST
        ) /
          ((hometots$FG / hometots$MP) * MP *
             5 - FG
          )) * (1 - (
            MP / (hometots$MP / 5)
          ))),
      AST_Part = 0.5 * (((
        hometots$PTS - hometots$FT
      ) - (PTS - FT)) /
        (2 * (hometots$FGA - FGA))) *
        AST,
      FT_Part = (1 - (1 - (FT / FTA)) ^ 2) *
        0.4 * FTA,
      Team_Scoring_Poss = hometots$FG + (1 -
                                           (1 - (
                                             hometots$FT / hometots$FTA
                                           )) ^ 2) * hometots$FTA * 0.4,
      `Team_ORB%` = hometots$ORB / (hometots$ORB + (awaytots$TRB - awaytots$ORB)),
      `Team_Play%` = Team_Scoring_Poss / (hometots$ORB + hometots$FTA * 0.4 + hometots$TOV),
      
      Team_ORB_Weight = ((1 - `Team_ORB%`) *
                           `Team_Play%`) / ((1 - `Team_ORB%`) *
                                              `Team_Play%` +
                                              `Team_ORB%` * (1 - `Team_Play%`)
                           ),
      ORB_Part = ORB * Team_ORB_Weight * `Team_Play%`,
      `FG_Part` = FG * (1 - 0.5 * ((PTS - FT) /
                                     (2 * FGA)) * qAST),
      FGxPoss = (FGA - FG) * (1 - 1.07 * `Team_ORB%`),
      FTxPoss = ((1 - (FT / FTA)) ^ 2) * 0.4 * FTA
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      ScPoss = (FG_Part + AST_Part + FT_Part) * (
        1 - (hometots$ORB / Team_Scoring_Poss) *
          Team_ORB_Weight * `Team_Play%`
      ) + ORB_Part,
      TotPoss = ScPoss + FGxPoss + FTxPoss + TOV,
      PProd_FG_Part = 2 * (FG + 0.5 * `X3P`) * (1 - 0.5 * ((PTS - FT) /
                                                             (2 * FGA)) * qAST),
      PProd_AST_Part = 2 * ((
        hometots$FG - FG + 0.5 * (hometots$`X3P` - `X3P`)
      ) /
        (hometots$FG - FG)) * 0.5 * (((
          hometots$PTS - hometots$FT
        ) - (PTS - FT)) / (2 * (hometots$FGA - FGA))) * AST,
      PProd_ORB_Part = ORB * Team_ORB_Weight * `Team_Play%` * (hometots$PTS /
                                                                 (hometots$FG + (1 - (
                                                                   1 - (hometots$FT / hometots$FTA)
                                                                 ) ^ 2) * 0.4 * hometots$FTA))
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      PProd = (PProd_FG_Part + PProd_AST_Part + FT) * (
        1 - (hometots$ORB / Team_Scoring_Poss)
        * Team_ORB_Weight *
          `Team_Play%`
      ) + PProd_ORB_Part
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      `DOR%` = awaytots$ORB / (awaytots$ORB + hometots$DRB),
      `DFG%` = awaytots$FG / awaytots$FGA,
      FMwt = (`DFG%` * (1 - `DOR%`)) / (`DFG%` * (1 - `DOR%`) + (1 -
                                                                   `DFG%`) * `DOR%`),
      Stops1 = STL + BLK * FMwt * (1 - 1.07 * `DOR%`) + DRB * (1 -
                                                                 FMwt),
      Stops2 = (((awaytots$FGA - awaytots$FG - hometots$BLK) / hometots$MP
      ) * FMwt * (1 - 1.07 * `DOR%`) +
        ((awaytots$TOV - hometots$STL) / hometots$MP
        )) * MP +
        (PF / hometots$PF) * 0.4 * awaytots$FTA * (1 - (awaytots$FT /
                                                          awaytots$FTA)) ^ 2,
      Stops = Stops1 + Stops2,
      `Stop%` = (Stops * awaytots$MP) / (homepos * MP),
      Team_Defensive_Rating = 100 * (hometots$PTS / homepos),
      D_Pts_per_ScPoss = awaytots$PTS / (awaytots$FG + (1 - (
        1 - (awaytots$FT / awaytots$FTA)
      ) ^ 2) * awaytots$FTA * 0.4)
    )
  
  a_imdt <- awaystats %>% mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      qAST = ((MP / (awaytots$MP / 5)) *
                (1.14 * ((awaytots$AST - AST) / awaytots$FG
                ))) +
        ((((awaytots$AST / awaytots$MP) * MP * 5 - AST
        ) /
          ((awaytots$FG / hometots$MP) * MP * 5 - FG
          )) * (1 - (
            MP / (awaytots$MP / 5)
          ))),
      AST_Part = 0.5 * (((
        awaytots$PTS - awaytots$FT
      ) - (PTS - FT)) /
        (2 * (awaytots$FGA - FGA))) * AST,
      FT_Part = (1 - (1 - (FT / FTA)) ^ 2) * 0.4 * FTA,
      Team_Scoring_Poss = awaytots$FG + (1 - (1 - (
        awaytots$FT / awaytots$FTA
      )) ^ 2) * awaytots$FTA * 0.4,
      `Team_ORB%` = awaytots$ORB / (awaytots$ORB + (hometots$TRB - hometots$ORB)),
      `Team_Play%` = Team_Scoring_Poss / (awaytots$ORB + awaytots$FTA * 0.4 + awaytots$TOV),
      
      Team_ORB_Weight = ((1 - `Team_ORB%`) * `Team_Play%`) / ((1 -
                                                                 `Team_ORB%`) *
                                                                `Team_Play%` +
                                                                `Team_ORB%` * (1 - `Team_Play%`)
      ),
      ORB_Part = ORB * Team_ORB_Weight * `Team_Play%`,
      `FG_Part` = FG * (1 - 0.5 * ((PTS - FT) / (2 * FGA)) * qAST),
      FGxPoss = (FGA - FG) * (1 - 1.07 * `Team_ORB%`),
      FTxPoss = ((1 - (FT / FTA)) ^ 2) * 0.4 * FTA
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      ScPoss = (FG_Part + AST_Part + FT_Part) * (
        1 - (awaytots$ORB / Team_Scoring_Poss) *
          Team_ORB_Weight * `Team_Play%`
      ) + ORB_Part,
      TotPoss = ScPoss + FGxPoss + FTxPoss + TOV,
      PProd_FG_Part = 2 * (FG + 0.5 * `X3P`) * (1 - 0.5 * ((PTS - FT) /
                                                             (2 * FGA)) * qAST),
      PProd_AST_Part = 2 * ((
        awaytots$FG - FG + 0.5 * (awaytots$`X3P` - `X3P`)
      ) /
        (awaytots$FG - FG)) * 0.5 * (((
          awaytots$PTS - awaytots$FT
        ) - (PTS - FT)) / (2 * (awaytots$FGA - FGA))) * AST,
      PProd_ORB_Part = ORB * Team_ORB_Weight * `Team_Play%` * (awaytots$PTS /
                                                                 (awaytots$FG +
                                                                    (1 - (
                                                                      1 - (awaytots$FT / awaytots$FTA)
                                                                    ) ^ 2) * 0.4 * awaytots$FTA))
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      PProd = (PProd_FG_Part + PProd_AST_Part + FT) * (
        1 - (awaytots$ORB / Team_Scoring_Poss)
        * Team_ORB_Weight *
          `Team_Play%`
      ) + PProd_ORB_Part
    ) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(
      `DOR%` = hometots$ORB / (hometots$ORB + awaytots$DRB),
      `DFG%` = hometots$FG / hometots$FGA,
      FMwt = (`DFG%` * (1 - `DOR%`)) / (`DFG%` * (1 - `DOR%`) + (1 -
                                                                   `DFG%`) * `DOR%`),
      Stops1 = STL + BLK * FMwt * (1 - 1.07 * `DOR%`) + DRB * (1 -
                                                                 FMwt),
      Stops2 = (((hometots$FGA - hometots$FG - awaytots$BLK) / awaytots$MP
      ) * FMwt * (1 - 1.07 * `DOR%`) +
        ((hometots$TOV - awaytots$STL) / awaytots$MP
        )) * MP +
        (PF / awaytots$PF) * 0.4 * hometots$FTA * (1 - (hometots$FT /
                                                          hometots$FTA)) ^ 2,
      Stops = Stops1 + Stops2,
      `Stop%` = (Stops * hometots$MP) / (awaypos * MP),
      Team_Defensive_Rating = 100 * (awaytots$PTS / awaypos),
      D_Pts_per_ScPoss = hometots$PTS / (hometots$FG + (1 - (
        1 - (hometots$FT / hometots$FTA)
      ) ^ 2) * hometots$FTA * 0.4)
    )
  
  
  advhome <- h_imdt %>% mutate(
    `TS.` = PTS / (2 * (FGA + 0.44 * FTA)),
    `eFG.` = (FG + 0.5 * `X3P`) / FGA,
    `3PAr` = `X3PA` / (FGA),
    `FTr` = FT / FGA,
    `ORB%` = 100 * (ORB * hometots$MP / 5) /
      (MP * (hometots$ORB + awaytots$DRB)),
    `DRB%` = 100 * (DRB * hometots$MP / 5) /
      (MP * (hometots$DRB + awaytots$ORB)),
    `TRB%` = 100 * (TRB * hometots$MP / 5) /
      (MP * (hometots$TRB + awaytots$TRB)),
    `AST%` = 100 * (AST) / (((
      MP / (hometots$MP / 5)
    ) * hometots$FG) - FG),
    `STL%` = 100 * (STL * hometots$MP / 5) /
      (MP * awaypos),
    `BLK%` = 100 * (BLK * hometots$MP / 5) /
      (MP * (awaytots$`X2PA`)),
    `TOV%` = 100 * (TOV) / (FGA + 0.44 * FTA +
                              TOV),
    `USG%` = 100 * ((FGA + 0.44 * FTA + TOV) *
                      (hometots$MP / 5)) / (MP * (
                        hometots$FGA + 0.44 * hometots$FTA + hometots$TOV
                      )),
    `ORtg` = 100 * (PProd / TotPoss),
    `DRtg` = Team_Defensive_Rating + 0.2 *
      (100 * D_Pts_per_ScPoss * (1 - `Stop%`) - Team_Defensive_Rating)
  ) %>%
    select((ncol(.) - 13):ncol(.)) %>% mutate_all( ~ round(., 3))
  advhome <-
    cbind(collectedbox$gamedata$home,
          homestats$Name,
          homestats$MP,
          advhome)
  colnames(advhome)[1:3] <- c("team", "Name", "MP")
  
  advaway <- a_imdt %>% mutate(
    `TS.` = PTS / (2 * (FGA + 0.44 * FTA)),
    `eFG.` = (FG + 0.5 * `X3P`) / FGA,
    `3PAr` = `X3PA` / (FGA),
    `FTr` = FT / FGA,
    `ORB%` = 100 * (ORB * awaytots$MP / 5) /
      (MP * (awaytots$ORB + hometots$DRB)),
    `DRB%` = 100 * (DRB * awaytots$MP / 5) /
      (MP * (awaytots$DRB + hometots$ORB)),
    `TRB%` = 100 * (TRB * awaytots$MP / 5) /
      (MP * (awaytots$TRB + hometots$TRB)),
    `AST%` = 100 * (AST) / (((
      MP / (awaytots$MP / 5)
    ) * awaytots$FG) - FG),
    `STL%` = 100 * (STL * awaytots$MP / 5) /
      (MP * homepos),
    `BLK%` = 100 * (BLK * awaytots$MP / 5) /
      (MP * (hometots$`X2PA`)),
    `TOV%` = 100 * (TOV) / (FGA + 0.44 * FTA +
                              TOV),
    `USG%` = 100 * ((FGA + 0.44 * FTA + TOV) *
                      (awaytots$MP / 5)) / (MP * (
                        awaytots$FGA + 0.44 * awaytots$FTA + awaytots$TOV
                      )),
    `ORtg` = 100 * (PProd / TotPoss),
    `DRtg` = Team_Defensive_Rating + 0.2 * (100 *
                                              D_Pts_per_ScPoss * (1 - `Stop%`) - Team_Defensive_Rating)
  ) %>%
    select((ncol(.) - 13):ncol(.)) %>% mutate_all( ~ round(., 3))
  advaway <-
    cbind(collectedbox$gamedata$away,
          awaystats$Name,
          awaystats$MP,
          advaway)
  colnames(advaway)[1:3] <- c("team", "Name", "MP")
  
  awayteam <-
    c(
      `TS.` = awaytots$PTS / (2 * (awaytots$FGA + 0.44 * awaytots$FTA)),
      `eFG.` = (awaytots$FG + 0.5 * awaytots$`X3P`) / awaytots$FGA,
      `3PAr` = awaytots$`X3PA` / (awaytots$FGA),
      `FTr` = awaytots$FTA / awaytots$FGA,
      `ORB%` = 5 * 100 * (awaytots$ORB * awaytots$MP / 5) / (awaytots$MP *
                                                               (awaytots$ORB + hometots$DRB)),
      `DRB%` = 5 * 100 * (awaytots$DRB * awaytots$MP / 5) / (awaytots$MP *
                                                               (awaytots$DRB + hometots$ORB)),
      `TRB%` = 5 * 100 * (awaytots$TRB * awaytots$MP / 5) / (awaytots$MP *
                                                               (awaytots$TRB + hometots$TRB)),
      `AST%` = 5 * 100 * (awaytots$AST) / (((
        awaytots$MP / (awaytots$MP / 5)
      ) * awaytots$FG) - awaytots$FG),
      `STL%` = 5 * 100 * (awaytots$STL * awaytots$MP / 5) / (awaytots$MP *
                                                               homepos),
      `BLK%` = 5 * 100 * (awaytots$BLK * awaytots$MP / 5) / (awaytots$MP *
                                                               (hometots$`X2PA`)),
      `TOV%` = 5 * 100 * (awaytots$TOV) / (awaytots$FGA + 0.44 *
                                             awaytots$FTA + awaytots$TOV),
      `USG%` = 5 * 100 * ((awaytots$FGA + 0.44 * awaytots$FTA + awaytots$TOV) *
                            (awaytots$MP / 5)
      ) /
        (
          awaytots$MP * (awaytots$FGA + 0.44 * awaytots$FTA + awaytots$TOV)
        ),
      `ORtg` = 100 * (awaytots$PTS / awaypos),
      `DRtg` = 100 * (hometots$PTS / homepos)
    ) %>%
    round(3) %>% t() %>% data.frame()
  
  awayteam <-
    data.frame(
      team = collectedbox$gamedata$away,
      Name = paste0(collectedbox$gamedata$away, ' Totals'),
      MP = awaytots$MP,
      awayteam,
      stringsAsFactors = FALSE
    )
  
  hometeam <-
    c(
      `TS.` = hometots$PTS / (2 * (hometots$FGA + 0.44 * hometots$FTA)),
      `eFG.` = (hometots$FG + 0.5 * hometots$`X3P`) /
        hometots$FGA,
      `3PAr` = hometots$`X3PA` / (hometots$FGA),
      `FTr` = hometots$FTA / hometots$FGA,
      `ORB%` = 5 * 100 * (hometots$ORB * hometots$MP /
                            5) / (hometots$MP * (hometots$ORB + awaytots$DRB)),
      `DRB%` = 5 * 100 * (hometots$DRB * hometots$MP /
                            5) / (hometots$MP * (hometots$DRB + awaytots$ORB)),
      `TRB%` = 5 * 100 * (hometots$TRB * hometots$MP /
                            5) / (hometots$MP * (hometots$TRB + awaytots$TRB)),
      `AST%` = 5 * 100 * (hometots$AST) / (((
        hometots$MP / (hometots$MP / 5)
      ) * hometots$FG) - hometots$FG),
      `STL%` = 5 * 100 * (hometots$STL * hometots$MP /
                            5) / (hometots$MP * awaypos),
      `BLK%` = 5 * 100 * (hometots$BLK * hometots$MP /
                            5) / (hometots$MP * (awaytots$`X2PA`)),
      `TOV%` = 5 * 100 * (hometots$TOV) / (hometots$FGA +
                                             0.44 * hometots$FTA + hometots$TOV),
      `USG%` = 5 * 100 * ((hometots$FGA + 0.44 *
                             hometots$FTA + hometots$TOV) * (hometots$MP / 5)
      ) /
        (
          hometots$MP * (hometots$FGA + 0.44 * hometots$FTA + hometots$TOV)
        ),
      `ORtg` = 100 * (hometots$PTS / homepos),
      `DRtg` = 100 * (awaytots$PTS / awaypos)
    ) %>%
    round(3) %>% t() %>% data.frame()
  hometeam <-
    data.frame(
      team = collectedbox$gamedata$home,
      Name = paste0(collectedbox$gamedata$home, ' Totals'),
      MP = hometots$MP,
      hometeam,
      stringsAsFactors = FALSE
    )
  colnames(hometeam) <- colnames(awayteam) <- colnames(advaway)
  return(
    data.frame(
      gameid = collectedbox$gamedata$gameid,
      rbind(hometeam, awayteam, advaway, advhome),
      stringsAsFactors = FALSE
    )
  )
}

#test it out
# collect_box('Kansas', 'Oklahoma', dt = '2019-01-02') %>% advanced_box_stats() %>% View()

#collect advanced stats for all games
idlist <-
  games %>% select(gameid) %>% collect() %>% unique %>% unlist()
alladv <-
  idlist %>% map(function(x)
    collect_box(game_id = x)) %>% map(advanced_box_stats) %>% bind_rows()
playeradv <- alladv %>% filter(!str_detect(Name, 'Totals'))
dbWriteTable(ncaa, 'playeradvancedgame', playeradv)
teamadv <- alladv %>% filter(str_detect(Name, 'Totals'))
dbWriteTable(ncaa, 'teamadvancedgame', teamadv)
dbDisconnect(ncaa)
ncaa <- dbConnect(RSQLite::SQLite(), "ncaa2018-19.sqlite")
#ncaa <- src_sqlite("ncaa2018-2019.sqlite")
# 
# games <- tbl(ncaa, "games")
# homes <- tbl(ncaa, 'homebox')
# aways <- tbl(ncaa, 'awaybox')
# advanced <- tbl(ncaa, 'playeradvancedgame')
# teamadvanced <- tbl(ncaa, 'teamadvancedgame')
