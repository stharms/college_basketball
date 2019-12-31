prep_boxes <- function(listin) {
  outdf_home <- listin %>% map(function(x)
    x$home = suppressWarnings(data.frame(
      team = x[['meta']][1], x[['home']],
      stringsAsFactors = FALSE
    ))) %>%
    bind_rows(.id = 'gameid') %>% as_tibble() %>%
    mutate(gameid = paste0((
      listin[[1]]$meta['date'] %>% str_remove_all('-') %>% as.numeric()
    ), gameid))
  
  outdf_away <- listin %>% map(function(x)
    x$away = suppressWarnings(data.frame(
      team = x[['meta']][3], x[['away']],
      stringsAsFactors = FALSE
    ))) %>%
    bind_rows(.id = 'gameid') %>% as_tibble() %>%
    mutate(gameid = paste0((
      listin[[1]]$meta['date'] %>% str_remove_all('-') %>% as.numeric()
    ), gameid))
  
  meta_rows <- listin %>% pluck('meta') %>%
    map(as.data.frame.list, stringsAsFactors = FALSE) %>%
    bind_rows(.id = 'gameid') %>%
    mutate(gameid = paste0((
      listin[[1]]$meta['date'] %>% str_remove_all('-') %>% as.numeric()
    ), gameid))
  
  return(list(
    gameinfo = meta_rows,
    home = outdf_home,
    away = outdf_away
  ))
}

