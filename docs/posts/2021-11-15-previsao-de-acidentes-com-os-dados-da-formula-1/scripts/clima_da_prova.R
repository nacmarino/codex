
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(rvest)
library(purrr)
library(xml2)
library(fs)

# carregando dados ------------------------------------------------------------------

## extraindo os paths das copias locais
paths_copias_locais <- dir_ls(path = '_posts/2021-09-11-f1-acidentes/data/', regexp = '.rds')

## criando vetor de nomes dos arquivos
nomes_arquivos <- paths_copias_locais %>% 
  path_file() %>% 
  path_ext_remove()

## carregando os arquivos em uma lista
tt_dataset <- map(.x = paths_copias_locais, .f = read_rds)

## renomeando os elementos da lista
names(tt_dataset) <- nomes_arquivos

## pegando apenas os links de cada prova
provas <- tt_dataset$races

# criando repo temporario -----------------------------------------------------------

## definindo o path que sera criado
temp_path <- '_posts/2021-09-11-f1-acidentes/scripts/temp'

## criando pasta temporaria caso necessario
if(!dir_exists(temp_path)){
  dir_create(path = temp_path, recurse = TRUE)
}

# pegando as paginas ----------------------------------------------------------------

## definindo uma funcao para pegar uma pagina
pega_uma_pagina <- function(target_url, raceid, path_to_save) {
  GET(
    url = target_url, 
    write_disk(path = sprintf(fmt = '%s/raceId_%04d.html', path_to_save, raceid), overwrite = TRUE)
  )
  Sys.sleep(time = runif(n = 1, min = 1, max = 2))
}

## pegando as paginas disponiveis
walk2(.x = pull(provas, url), 
      .y = pull(provas, raceId), 
      .f = ~ pega_uma_pagina(target_url = .x, raceid = .y, path_to_save = temp_path))

# parseando as paginas --------------------------------------------------------------

## definindo os paths para os arquivos
temp_files <- dir_ls(path = temp_path)

## criando uma funcao para fazer o parser de uma pagina
parse_uma_pagina <- function(file_path){
  read_html(x = file_path) %>% 
    xml_find_first(xpath = '//table[@class="infobox vevent"]//tbody') %>% 
    html_table() %>% 
    .[,c(1,2)] %>% 
    set_names(nm = c('variavel', 'valor')) %>% 
    filter(variavel %in% c('Course', 'Course length', 'Distance', 'Weather'))
}

## amarrando a funcao para parsear uma pagina em um possibly
possibly_parse_uma_pagina <- possibly(.f = parse_uma_pagina, otherwise = tibble())

## pegando todos os dados num tibble
df <- map(.x = temp_files, .f = possibly_parse_uma_pagina) %>% 
  enframe(name = 'raceId', value = 'resultados')

## removendo casos em que falhamos pegar as informacoes
df <- df %>% 
  # estes serao os casos que temos zero linhas na tibble resultante
  mutate(linhas = map_dbl(.x = resultados, .f = nrow)) %>% 
  # removendo os casos com zero linhas
  filter(linhas != 0) %>% 
  # dropando a coluna calculada
  select(-linhas)

# faxinando os dados ----------------------------------------------------------------

## desempacotando e tratando os dados
df <- df %>% 
  # desaninhando os tibbles na coluna
  unnest(cols = resultados) %>% 
  # passando base para o formato largo
  pivot_wider(id_cols = raceId, names_from = variavel, values_from = valor) %>% 
  # tratando as colunas
  mutate(
    # extraindo o valor do raceId
    raceId = str_extract(string = raceId, pattern = '(?<=raceId_)[0-9]{4}(?=\\.html)'),
    # extraindo a extensão do circuito
    extensao = str_extract(string = `Course length`, pattern = '^.+(?=\\skm)'),
    extensao = str_replace(string = extensao, pattern = '\\.', replacement = ','),
    # extraindo a quantidade de voltas do circuito
    voltas = str_extract(string = Distance, pattern = '^[0-9]+(?=\\slaps)'),
    # extraindo a distancia percorrida na prova
    distancia = str_extract(string = Distance, pattern = '(?<=,\\s).+(?=\\skm)'),
    distancia = str_replace(string = distancia, pattern = '\\.', replacement = ','),
    # criando features para o tipo de circuito
    street_circuit = str_detect(string = Course, pattern = '(?i)street circuit'),
    permanent_circuit = str_detect(string = Course, pattern = '(?i)permanent'),
    temporary_circuit = str_detect(string = Course, pattern = '(?i)temporary'),
    # parseando o que for numerico para numerico
    raceId = parse_number(raceId),
    extensao = parse_number(extensao),
    voltas = parse_number(voltas),
    distancia = parse_number(distancia)
  ) %>% 
  select(-Course, -`Course length`, -Distance)
df

## codando informacoes do tempo
df <- df %>% 
  # usando regex para pegar as informacoes do tempo
  mutate(
    nublado = str_detect(string = Weather, pattern = '(?i)cloud[ys]?|overcast'),
    frio = str_detect(string = Weather, pattern = '(?i)chilly|cold|cool'),
    seco = str_detect(string = Weather, pattern = '(?i)dry'),
    umido = str_detect(string = Weather, pattern = '(?i)damp|humid(?:ity)?|wet'),
    chuva = str_detect(string = Weather, pattern = '(?i)precipitation|rain|shower|thunderstorm|torrential|monsoon|drizzl[ey]'),
    ensolarado = str_detect(string = Weather, pattern = '(?i)sunny|clear|fine'),
    quente = str_detect(string = Weather, pattern = '(?i)hot|warm')
  ) %>% 
  # removendo a coluna de texto do tempo
  select(-Weather) %>% 
  # parseando os logicos para numericos
  mutate(across(where(is.logical), as.numeric)) 

# exportando os dados ---------------------------------------------------------------

write_rds(x = df, file = '_posts/2021-09-11-f1-acidentes/data/weather_and_other.rds')
