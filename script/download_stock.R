library(purrr)

get_prices <- function(ticker, ini_date, end_date)
{
  
  `%>%` <- magrittr::`%>%`
  
  db <- 
    BatchGetSymbols::BatchGetSymbols(
      tickers    = ticker,
      first.date = ini_date,
      last.date  = end_date,
      do.cache = F
    )
  
  if(length(db$df.control) == 0)
  {
    return(NA_character_)
  }else
  {
    db %>% 
      .$df.tickers %>% 
      janitor::clean_names() %>% 
      return()
  }
  
}

stickers <- c('PETR4.SA', 'ABEV3.SA', 'VVAR3.SA', 'ITSA4.SA', 'USIM5.SA', 'SUZB3.SA')


end_date <- Sys.Date()

ini_date <- Sys.Date() - lubridate::years(1)

df1 <- map(stickers, get_prices, ini_date = ini_date, end_date = end_date)

df2 <- map_df(stickers, get_prices, ini_date = ini_date, end_date = end_date)


