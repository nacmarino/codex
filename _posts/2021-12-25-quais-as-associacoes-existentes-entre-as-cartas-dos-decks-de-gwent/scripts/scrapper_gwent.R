
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(rvest)
library(purrr)
library(xml2)
library(jsonlite)
library(fs)
library(lubridate)
library(furrr)

# criando repo temporario -----------------------------------------------------------

## definindo os paths que serao criados
### para as listas de decks
list_path <- '_posts/2021-12-25-quais-as-associacoes-existentes-entre-as-cartas-dos-decks-de-gwent/temporario_lista'
### para os decks
deck_path <- '_posts/2021-12-25-quais-as-associacoes-existentes-entre-as-cartas-dos-decks-de-gwent/temporario_deck'

## criando pasta temporaria caso necessario
if(!dir_exists(list_path)){
  dir_create(path = list_path, recurse = TRUE)
}

## criando pasta temporaria caso necessario
if(!dir_exists(deck_path)){
  dir_create(path = deck_path, recurse = TRUE)
}

# funcoes ---------------------------------------------------------------------------

## para raspar uma lista
scrape_lista <- function(pagina, listas_por_pagina, path_to_save) {
  ## calculando o offset atraves da pagina que deseja-se pegar
  offset <- (pagina * listas_por_pagina) - listas_por_pagina
  ## fazendo o request e salvando em disco
  GET(
    url = str_glue('https://www.playgwent.com/pt-BR/decks/api/guides/offset/{offset}/limit/{listas_por_pagina}'), 
    write_disk(path = sprintf(fmt = '%s/pagina_%06d.json', path_to_save, pagina), overwrite = TRUE)
  )
  Sys.sleep(time = runif(n = 1, min = 1e-2, max = 1))
}

## para parseaar uma lista
parser_lista <- function(path_para_lista) {
  # carregando os dados como um json
  read_json(path = path_para_lista, simplifyDataFrame = TRUE) %>% 
    # pegando so o elemento da lista que sejam os guides
    pluck('guides') %>% 
    # passando para tibble
    as_tibble() %>% 
    # desempacotando o dataframe column para multiplas colunas
    unpack(cols = faction) %>% 
    # jogando fora o que nao interessa
    select(-c(short, thumbnailImg, abilityImg))
}

## para raspar um deck
scrape_deck <- function(deck_id, path_to_save) {
  ## fazendo o request e salvando em disco
  GET(
    url = str_glue('https://www.playgwent.com/pt-BR/decks/guides/{deck_id}'), 
    write_disk(path = sprintf(fmt = '%s/deck_%07d.html', path_to_save, deck_id), overwrite = TRUE)
  )
  Sys.sleep(time = runif(n = 1, min = 1, max = 2))
}

# para parsear o html do deck para o json
deck_to_json <- function(path_para_deck) {
  # lendo o arquivo salvo como html
  read_html(x = path_para_deck) %>% 
    # pegando o xpath onde está o dicionario json
    xml_find_all(xpath = '//div[@class="wrapper"]//div[@class="content"]//div[@id="root"]') %>% 
    # pegando o atributo do dicionario
    xml_attr(attr = 'data-state') %>% 
    # parseando o json
    jsonlite::fromJSON(simplifyDataFrame = TRUE) %>% 
    # pegando o guide
    pluck('guide') %>% 
    # passando tudo para um dataframe
    enframe %>% 
    # pegando so o deck
    filter(name == 'deck') %>% 
    # desaninhando a list column
    unnest(value) %>% 
    # pegando os valores
    pull(value)
}

# para fazer o parser do tooltip de cada carta
parser_card_tooltip <- function(deck_json_file, card_type = c('cards', 'stratagem', 'leader')) {
  
  # procedimento para pegar os dados do tooltip para a carta de habilidade lider
  if(card_type[1] == 'leader') {
    # pegando o tooltip da habilidade do lider e juntando tudo em um tibble so
    tooltip_data <- deck_json_file %>% 
      pluck('leader', 'tooltip') %>% 
      map(.f = bind_rows) %>% 
      bind_rows() 
    # procedimento para pegar os dados do tooltip para as cartas normais ou de estrategia
  } else {
    tooltip_data <- deck_json_file %>% 
      pluck(card_type[1], 'tooltip') %>% 
      map(.f = bind_rows) %>% 
      enframe(name = 'card_in_seq') %>% 
      unnest(cols = c(value)) %>% 
      group_by(card_in_seq) 
  }
  
  # summarizando os dados de cada tooltip para cada carta
  tooltip_data <- tooltip_data %>% 
    summarise(
      keywords = paste0(unique(str_to_lower(string = key[!is.na(key)])), collapse = ';'),
      texto    = paste0(value, collapse = ' ')
    ) %>% 
    # tratando o texto do tooltip
    mutate(
      keywords = ifelse(test = keywords == '', yes = NA, no = keywords),
      keywords = str_to_lower(string = keywords),
      texto = str_replace_all(string = texto, pattern = '\\s+', replacement = ' '),
      texto = str_replace_all(string = texto, pattern = '\\s:', replacement = ':'),
      texto = str_replace_all(string = texto, pattern = '(?<=\\()\\s|\\s(?=\\))', replacement = ''),
      texto = str_replace_all(string = texto, pattern = '\\s(?=\\.)|\\s(?=\\,)', replacement = ''),
    )
  
  # adicionando identificador numerico para a carta do tooltip e retornando o resultado
  if(card_type[1] == 'cards') {
    tooltip_data
  }
  else{
    mutate(tooltip_data,
           card_in_seq = if(card_type[1] == 'leader') {-1L} else {0L}
    ) %>% 
      relocate(card_in_seq, .before = keywords)
  }
}

# para parsear os metadados de cada carta
parser_card_metadata <- function(deck_json_file, card_type = c('cards', 'stratagem', 'leader')) {
  # pegando os metadados da carta
  card_metadata <- deck_json_file %>% 
    # selecionando o tipo de carta
    pluck(card_type[1]) %>% 
    # descartando informacoes que nao precisamos
    discard(names(.) %in% c('slotImg', 'slotImgCn', 'previewImgCn', 'thumbnailImg', 'thumbnailImgCn',
                            'abilityImg', 'abilityImgCn', 'tooltip')) 
  
  if(card_type[1] == 'cards') {
    card_metadata %>% 
      as_tibble() %>% 
      unpack(cols = c(faction, previewImg)) %>% 
      add_column(card_in_seq = 1:nrow(.), .before = 'craftingCost')
  } else {
    card_metadata %>% 
      bind_cols() %>% 
      suppressWarnings() %>% 
      mutate(card_in_seq = if(card_type[1] == 'leader') {-1L} else {0L}) %>% 
      relocate(card_in_seq, .before = craftingCost)
  }
}

# para parsear todas as informacoes do deck
parser_cards <- function(path_para_deck) {
  # parseando o json do deck
  target_deck <- deck_to_json(path_para_deck = path_para_deck)
  
  # pegando os tooltips de todas as cartas
  tooltips <- map_dfr(.x = c('leader', 'stratagem', 'cards'),
                      .f = parser_card_tooltip,
                      deck_json_file = target_deck)
  
  # pegando os metadados de todas as cartas
  metadados <- map_dfr(.x = c('leader', 'stratagem', 'cards'),
                       .f = parser_card_metadata,
                       deck_json_file = target_deck)
  
  left_join(x = metadados, y = tooltips, by = 'card_in_seq')
}

# pegando a lista de decks disponiveis ----------------------------------------------

## pegando 10000 decks disponiveis
walk(.x = 1:39,
     .f = scrape_lista,
     listas_por_pagina = 1000, path_to_save = list_path)

## listando o path para os decks disponiveis
listas_disponiveis <- dir_ls(path = list_path, regexp = '.json')

## lendo todos os decks
plan(multisession)
todos_os_decks <- map_dfr(.x = listas_disponiveis, .f = parser_lista)
plan(sequential)

## extraindo os decks que vamos buscar e raspar
decks_selecionados <- todos_os_decks %>% 
  filter(votes > 0) %>% 
  filter(year(as_datetime(x = modified)) ==  2021) %>% 
  arrange(desc(votes))
decks_selecionados

# pegando os decks selecionados -----------------------------------------------------

## pegando os decks selecionados
walk(.x = decks_selecionados$id,
     .f = scrape_deck,
     path_to_save = deck_path)

## listando todos os raspados
decks_disponiveis <- dir_ls(path = deck_path, regexp = '.html')

# parseando todos os decks
plan(multisession)
decks <- future_map(.x = decks_disponiveis, 
                    .f = possibly(parser_cards, otherwise = tibble()))
plan(sequential)

## juntando todos os decks em um arquivo so
decks <- decks %>% 
  # descartando listas vazias
  discard(is_empty) %>% 
  # juntando decks em um tibble so
  bind_rows(.id = 'deck') %>% 
  # ajustando id dos decks
  mutate(deck = str_extract(string = deck, pattern = '(?<=deck_)([0-9]+)'))

# inputando tipos missing
decks <- decks %>% 
  mutate(
    categoryName = case_when(is.na(categoryName) & type == 'leader' ~ 'Líder',
                             is.na(categoryName) & type == 'stratagem' ~ 'Estratégia',
                             TRUE ~ categoryName)
  )

# salvando decks --------------------------------------------------------------------

## salvando os decks
decks %>% 
  select(-where(is.list)) %>% 
  write_rds(file = str_glue('_posts/2021-12-25-quais-as-associacoes-existentes-entre-as-cartas-dos-decks-de-gwent/data/decks.rds'))

## salvando a lista de decks
todos_os_decks %>%
  write_rds(file = str_glue('_posts/2021-12-25-quais-as-associacoes-existentes-entre-as-cartas-dos-decks-de-gwent/data/lista_de_decks.rds'))
  