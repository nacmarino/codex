
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
    write_disk(path = sprintf(fmt = '%s/deck_%06d.html', path_to_save, deck_id), overwrite = TRUE)
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
    discard(names(.) %in% c('slotImg', 'slotImgCn', 'previewImg', 'previewImgCn', 'thumbnailImg', 'thumbnailImgCn',
                            'abilityImg', 'abilityImgCn', 'tooltip')) 
  
  if(card_type[1] == 'cards') {
    card_metadata %>% 
      as_tibble() %>% 
      unpack(cols = faction) %>% 
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

# pegando listas --------------------------------------------------------------------

## pegando uma lista
scrape_lista(pagina = 1, listas_por_pagina = 1000, path_to_save = list_path)

## pegando 4 listas
walk(.x = 2:5,
     .f = scrape_lista,
     listas_por_pagina = 1000, path_to_save = list_path)

# parseando listas ------------------------------------------------------------------

## listando o path para as listas
listas_disponiveis <- dir_ls(path = list_path, regexp = '.json')

## parseando uma lista
parser_lista(path_para_lista = listas_disponiveis[1])

## parseando muitas listas
map_dfr(.x = listas_disponiveis, .f = parser_lista)

# pegando decks ---------------------------------------------------------------------

## carregando uma paginas de listas
guides_1 <- parser_lista(path_para_lista = listas_disponiveis[1])

## raspando um deck
scrape_deck(deck_id = guides_1$id[1], path_to_save = deck_path)

## pegando 4 decks
walk(.x = guides_1$id[2:5],
     .f = scrape_deck,
     path_to_save = deck_path)

# parseando decks -------------------------------------------------------------------

## listando todos os decks
decks_disponiveis <- dir_ls(path = deck_path, regexp = '.html')

## parseando um deck
parser_cards(path_para_deck = decks_disponiveis[1])

# parseando todos os decks
map_dfr(.x = decks_disponiveis, 
        .f = parser_cards, 
        .id = 'deck') %>% 
  mutate(deck = str_extract(string = deck, pattern = '(?<=deck_)([0-9]+)'))
