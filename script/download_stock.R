library(purrr)
library(ggplot2)
#library(showtext) # pacote para pegar um fonte balaqueira

font_add_google("Montserrat", "Montserrat")

#---- function to get prices ----
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

#---- geting data ----

stickers <- c('PETR4.SA', 'ABEV3.SA', 'VVAR3.SA', 'ITSA4.SA', 'USIM5.SA', 'SUZB3.SA')

end_date <- Sys.Date()

ini_date <- Sys.Date() - lubridate::years(2)

df1 <- map(stickers,
           get_prices,
           ini_date = ini_date,
           end_date = end_date) %>% 
  bind_rows()

df2 <- map_df(stickers,
              get_prices,
              ini_date = ini_date,
              end_date = end_date)

#---- descriptive ----

glimpse(df1)


df1 %>% 
  filter(ticker == 'PETR4.SA') %>% 
  ggplot(aes(x = ref_date, y = price_close)) +
  geom_line(color = '#2c4e85', size = 1) +
  scale_x_date(breaks = ("1 months"), date_labels = '%m/%y') +
  scale_y_continuous(labels = scales::dollar_format(prefix = 'R$')) +
  labs(x = 'Data', y = 'Preço', title = unique(df1$ticker), caption = 'By: Marcolin') +
  theme_bw() + 
  theme(text            = element_text(family = "Montserrat"),
        axis.text.x     = element_text(angle = 45, vjust = 0.5, face = "bold"),
        axis.text.y     = element_text(face = "bold"),
        legend.title    = element_blank(),
        legend.position = c(0.4, 0.85),
        plot.title      = element_text(color = "#b3343e", size = 12, face = "bold"),
        plot.caption    = element_text(color = "#355496", face = "italic"))

#---- now with function ----

plot_stock <- function(df){
  
  ticker_name <- unique(df$ticker)
  
  df %>% 
    ggplot(aes(x = ref_date, y = price_close)) +
    geom_line(color = '#2c4e85', size = 1) +
    scale_x_date(breaks = ("1 months"), date_labels = '%m/%y') +
    scale_y_continuous(labels = scales::dollar_format(prefix = 'R$')) +
    labs(x = 'Data', y = 'Preço', title = ticker_name, caption = 'By: Marcolin') +
    theme_bw() + 
    theme(text            = element_text(family = "Montserrat"),
          axis.text.x     = element_text(angle = 45, vjust = 0.5, face = "bold"),
          axis.text.y     = element_text(face = "bold"),
          legend.title    = element_blank(),
          legend.position = c(0.4, 0.85),
          plot.title      = element_text(color = "#b3343e", size = 12, face = "bold"),
          plot.caption    = element_text(color = "#355496", face = "italic")) %>% 
    return()
}

plots <- df1 %>% 
  split(.$ticker) %>% 
  map(plot_stock)
