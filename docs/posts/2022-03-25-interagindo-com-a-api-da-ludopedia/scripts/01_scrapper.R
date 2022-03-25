
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

library(tidyverse)
library(jsonlite)
library(httr)
library(fs)

# criando pastas temporarias --------------------------------------------------------

## definindo os paths temporarios que serao usados
path_para_jogos <- '_posts/2022-03-02-interagindo-com-a-api-da-ludopedia/temporario_jogos'
path_para_metadata <- '_posts/2022-03-02-interagindo-com-a-api-da-ludopedia/temporario_metadados'
path_para_ranking <- '_posts/2022-03-02-interagindo-com-a-api-da-ludopedia/temporario_ranking'

## criando pasta para guardar a tabela com a lista de jogos
if(!dir_exists(path = path_para_jogos)){
  dir_create(path = path_para_jogos)
}

## criando pasta para guardar a tabela com os metadados dos jogos
if(!dir_exists(path = path_para_metadata)){
  dir_create(path = path_para_metadata)
}

## criando pasta para guardar a tabela com os metadados dos jogos
if(!dir_exists(path = path_para_ranking)){
  dir_create(path = path_para_ranking)
}

# definindo funcoes -----------------------------------------------------------------

# função para fazer a autenticacao do aplicativo
pegar_access_token <- function(APP, APP_ID, URIs) {
  
  # setando o endpoint de acordo com os dados fornecidos na ludopedia
  ludopedia_endpoint <- httr::oauth_endpoint(request = NULL,
                                             authorize = 'https://ludopedia.com.br/oauth/',
                                             access = 'https://ludopedia.com.br/tokenrequest/'
  )
  
  # setando as configurações do aplicativo conforme definido no site da ludopedia
  ludopedia_app <- httr::oauth_app(
    appname = APP,
    key = APP_ID,
    secret = NULL,
    redirect_uri = URIs
  )
  
  # pegando a autorizacao através do OAUTH2.0
  autorizacao <- httr::init_oauth2.0(endpoint = ludopedia_endpoint, 
                                     app = ludopedia_app)
  
  # criando string do token
  access_token <- paste0(autorizacao$token_type, ' ', autorizacao$access_token)
  
  # retornando o token de acesso
  return(access_token)
}

# função para pegar todos os jogos em uma dada página
pegar_lista_jogos <- function(pagina, access_token, path_salvar = NULL) {
  
  # pegando os jogos da pagina selecionada
  httr::GET('https://ludopedia.com.br/api/v1/jogos', 
            httr::add_headers(AUTHORIZATION = access_token),
            query = list(page = pagina, rows = 100),
            write_disk(path = stringr::str_glue(path_salvar, '/lista_jogos_pagina_{pagina}.json'),
                       overwrite = TRUE))
}

# função para parsear uma pagina contendo a lista de jogos cadastrados na base da Ludopedia
parser_lista_jogos <- function(target_path) {
  # carregando o arquivo json
  jsonlite::read_json(path = target_path, simplifyDataFrame = TRUE) %>% 
    # extraindo o elemento correspondente aos jogos
    pluck('jogos') %>% 
    # colocando como tibble
    as_tibble() %>% 
    # deixando a tibble desaninhada
    unnest(cols = everything())
}

# função para pegar a quantidade total de jogos disponiveis
pegar_total_jogos <- function(access_token) {
  # faz um GET da pagina default dos jogos, parseia o conteudo e pega so o elemento
  # da lista que contem a quantidade total de jogos cadastrados na base da Ludopedia
  httr::GET('https://ludopedia.com.br/api/v1/jogos', 
            httr::add_headers(AUTHORIZATION = access_token)) %>% 
    httr::content() %>% 
    purrr::pluck('total') %>% 
    parse_integer()
}

# função para pegar uma página do ranking da ludopedia
pegar_pagina_ranking <- function(pagina, path_salvar = NULL) {
  # fazendo o GET de uma pagina do ranking
  GET(url = paste0('https://www.ludopedia.com.br/ranking?pagina=', pagina),
      write_disk(path = stringr::str_glue(path_salvar, '/ranking_pagina_{pagina}.html'),
                 overwrite = TRUE)
  )
}

# função para fazer o parser da pagina do ranking
parser_pagina_ranking <- function(arquivo) {
  ## carregando o arquivo html contendo o response do GET da pagina do ranking
  pagina_do_ranking <- xml2::read_html(x = arquivo)
  # extrai as informacoes e coloca todos os jogos em uma tibble
  tibble::tibble(id_jogo = pagina_do_ranking %>% 
                   xml2::xml_find_all(xpath = '//*[@class="media-body"]/a[not(contains(@href, "ludopedia"))]') %>% 
                   xml2::xml_attr('data-id_jogo'),
                 nm_jogo = pagina_do_ranking %>% 
                   xml2::xml_find_all(xpath = '//*[@class="media-heading"]//@title') %>% 
                   xml2::xml_text(),
                 ano = pagina_do_ranking %>% 
                   xml2::xml_find_all(xpath = '//*[@class="media-heading"]/small') %>% 
                   xml2::xml_text(),
                 ranking = pagina_do_ranking %>% 
                   xml2::xml_find_all(xpath = '//*[@class="media-heading"]/*[@class="rank"]') %>% 
                   xml2::xml_text(),
                 notas = pagina_do_ranking %>% 
                   xml2::xml_find_all(xpath = '//*[@class="rank-info"]') %>% 
                   xml2::xml_text()
  ) %>% 
    # separa a string de nota em uma coluna para cada informacao de nota
    separate(col = notas, into = c('nota_rank', 'nota_media', 'n_notas', 'sua_nota'), sep = '\\|', remove = TRUE) %>% 
    # extraindo o valor numerico de todas as colunas em que elas foram tidas
    # e misturadas com caracteres
    mutate(
      ano        = str_extract(string = ano, pattern = '[0-9]+'),
      ranking    = str_extract(string = ranking, pattern = '[0-9]+'),
      nota_rank  = str_extract(string = nota_rank, pattern = '[0-9]+\\.[0-9]+'),
      nota_media = str_extract(string = nota_media, pattern = '[0-9]+\\.[0-9]+'),
      n_notas    = str_extract(string = n_notas, pattern = '[0-9]+')
    ) %>% 
    # passando todas as colunas numericas para tal
    mutate(across(ano:n_notas, parse_number)) %>% 
    # dropando a coluna com a nota de quem raspou
    select(-sua_nota)
}

# função para pegar a quantidade total de paginas no ranking disponiveis
pegar_total_ranking <- function(target_path) {
  # abre um arquivo HTML da pagina do ranking e extra a quantidade total de paginas existentes a partir dele
  xml2::read_html(x = target_path) %>% 
    xml2::xml_find_all(xpath = '//li//a[@title="Última Página"]') %>% 
    xml2::xml_attr('href') %>% 
    str_extract(pattern = '(?<=pagina=)[0-9]+') %>% 
    parse_integer()
}

# função para pegar as informações de um jogo
pegar_infos_jogo <- function(id_jogo, access_token, path_salvar = NULL) {
  # pegando os detalhes do jogo
  httr::GET(url = stringr::str_glue('https://ludopedia.com.br/api/v1/jogos/{id_jogo}'), 
            add_headers(AUTHORIZATION = access_token),
            write_disk(path = stringr::str_glue(path_salvar, '/info_do_jogo_{id_jogo}.json'),
                       overwrite = TRUE))
}

# função para parsear as informações de um jogo
parser_infos_jogo <- function(target_path){
  # carregando o arquivo JSON com os metadados do jogo
  metadados <- jsonlite::read_json(path = target_path, simplifyDataFrame = TRUE)
  # colocando todos os metadados em um tibble
  bind_cols(
    # parseando as informacoes que estao organizadas como strings ou numeros
    metadados %>% 
      # descartando todos os elementos que sao listas
      discard(is.list) %>% 
      # juntando cada elemento da lista coluna a coluna
      bind_cols(),
    # parseando as informacoes que estao organizadas como uma lista de dataframes
    metadados %>% 
      # retendo apenas os elementos que sao listas
      keep(is.list) %>% 
      # pegando a coluna que contem os o nome do metadado
      map(.f = pull, var = 2) %>% 
      # passando o vetor de strings para um unico string por elemento da lista
      map(.f = paste0, collapse = ';') %>% 
      # juntando os elementos da lista coluna a coluna
      bind_cols()
  )
}

# pegando o token -------------------------------------------------------------------

## criando um token de acesso para a sessao
meu_token <- pegar_access_token(APP = 'meu_app', APP_ID = Sys.getenv('app_id'), URIs = 'http://localhost:1410/')

# pegando pagina do ranking ---------------------------------------------------------

## pegando uma pagina do ranking
pegar_pagina_ranking(pagina = 1, path_salvar = path_para_ranking)

## definindo a quantidade total de paginas de ranking existentes
pegar_total_ranking(target_path = dir_ls(path = path_para_ranking)[1])

## faz o parser da pagina do ranking que esta no HTML
parser_pagina_ranking(arquivo = dir_ls(path = path_para_ranking)[1])

# pegando tabelas dos jogos ---------------------------------------------------------

## pegando uma pagina qualquer de exemplo
pegar_lista_jogos(pagina = 1, access_token = meu_token, path_salvar = path_para_jogos)

## parseando a pagina de um jogo
parser_lista_jogos(target_path = dir_ls(path = path_para_jogos)[1])

## pegando a quantidade total de jogos cadastrados
pegar_total_jogos(access_token = meu_token)

# pegando os metadados de um jogo ---------------------------------------------------

## pegando os metadados de um jogo
pegar_infos_jogo(id_jogo = 404, access_token = meu_token, path_salvar = path_para_metadata)

## parseando as informacoes do jogo selecionado
parser_infos_jogo(target_path = dir_ls(path = path_para_metadata)[2])
