
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

library(tidyverse)
library(httr)
library(xml2)

# carregando dados ------------------------------------------------------------------

## lendo os dados do ranking da BGG para pegar o identificador unico de cada jogo
ranking <- read_rds(file = '_posts/2022-01-09-interagindo-com-a-api-do-boardgamegeek/data/ranking_bgg.rds')

# definindo funcoes -----------------------------------------------------------------


# aplicando funcoes -----------------------------------------------------------------

base_url <- 'https://www.boardgamegeek.com/xmlapi2/'

foo <- GET(url = paste0(base_url, 'thing?id=', ranking$id[1]))

foo %>% 
  content() %>% 
  xml_children() %>% 
  xml_children() %>% 
  as_list()

foo <- GET(url = paste0(base_url, 'thing?id=', ranking$id[1], '&stats=1&marketplace=1&ratingcomments=1&page=2&pagesize=100'))

foo %>% 
  content() %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_name()

foo %>% 
  content() %>% 
  as_list() %>% 
  pluck('items', 'item', 'statistics')

foo %>% 
  content() %>% 
  as_list() %>% 
  pluck('items', 'item', 'comments') %>% 
  map_chr(attr, which = 'username') %>% 
  enframe

foo %>% 
  content() %>% 
  as_list() %>% 
  pluck('items', 'item', 'marketplacelistings') 
