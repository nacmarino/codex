
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(rvest)
library(purrr)
library(xml2)
library(fs)

# criando repo temporario -----------------------------------------------------------

## definindo os paths que serao criados
### para as listas de decks
list_path <- '_posts/2021-10-27-gwent/temporario_lista'
### para os decks
deck_path <- '_posts/2021-10-27-gwent/temporario_deck'

## criando pasta temporaria caso necessario
if(!dir_exists(list_path)){
  dir_create(path = list_path, recurse = TRUE)
}

## criando pasta temporaria caso necessario
if(!dir_exists(deck_path)){
  dir_create(path = deck_path, recurse = TRUE)
}

# setando url -----------------------------------------------------------------------

## url de uma pagina
base_url <- 'https://www.playgwent.com/pt-BR/decks/api/guides/offset/0/limit/1000'

## url do guide
guide_url <- 'https://www.playgwent.com/pt-BR/decks/guides/'

# pegando uma pagina ----------------------------------------------------------------

pg <- GET(
  url = base_url, 
  write_disk(path = sprintf(fmt = '%s/pagina_%06d.html', list_path, 1), overwrite = TRUE)
)

# analisando a pagina ---------------------------------------------------------------

parsed_init <- content(pg, simplifyDataFrame = TRUE) %>% 
  pluck('guides') %>% 
  tibble()

# pegando um deck -------------------------------------------------------------------

gg <- GET(
  url = paste0(guide_url, pull(parsed_init, id)[1]), 
  write_disk(path = sprintf(fmt = '%s/deck_%08d.html', deck_path, pull(parsed_init, id)[1]), overwrite = TRUE)
)

# analisando a pagina ---------------------------------------------------------------

read_html(x = '_posts/2021-10-27-gwent/temporario_deck/deck_00221339.html') %>% 
  xml_find_all(xpath = '//div[@class="wrapper"]//div[@class="content"]//div[@id="root"]') %>% 
  xml_attr(attr = 'data-state') %>% 
  jsonlite::fromJSON(simplifyDataFrame = TRUE) %>% 
  pluck('guide') %>% 
  enframe %>% 
  filter(name == 'deck') %>% 
  unnest(value) %>% 
  pull(value) %>% 
  # pluck('stratagem')
  # pluck('leader')
  pluck('cards') %>% pull(tooltip)

