
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse) # core
library(httr) # web scrapping
library(xml2) # parsear
library(fs) # mexer com paths
library(janitor) # para ajudar na faxina

# definindo objetos do ambiente -----------------------------------------------------

## para raspar o site
base_url <- 'https://boardgamegeek.com/browse/boardgame/page/'

## path do post
path_post <- '_posts/2021-10-24-as-notas-dos-rankings-dos-portais-de-jogos-de-tabuleiro-sao-similares/'

## path para diretorio temporairio para fazer o dump das paginas
path_temp <- paste0(path_post, 'temp_bgg')

## path para diretorio onde estarao os dados
path_data <- paste0(path_post, 'data')

# criando diretorios ----------------------------------------------------------------

## diretorio temporario para jogar os HTMLs raspados
if(!dir_exists(path = path_temp)){
  dir_create(path = path_temp, recurse = TRUE)
}

## diretorio para jogar o dado depois do parsing
if(!dir_exists(path = path_data)){
  dir_create(path = path_data, recurse = TRUE)
}

# definindo funções -----------------------------------------------------------------

# função para definir o número máximo de páginas para raspar
pega_max_paginas <- function(url_base) {
  GET(url = str_glue(url_base, 1)) %>% 
    # pegando o conteudo do GET
    content() %>% 
    # pegando a tag que contem o limite de paginas
    xml_find_first(xpath = '//div//*[@title="last page"]') %>% 
    # limpando a tag
    xml_text() %>% 
    # passando ela para um numero
    parse_number()
}

# função para fazer o GET
pega_pagina <- function(url_base, pagina, save_dir) {
  ## junta a base url com o numero da pagina e salva no diretorio alvo
  GET(url = str_glue(url_base, pagina), 
      write_disk(path = sprintf(fmt = '%s/pagina_%04d.html', save_dir, pagina), 
                 overwrite = TRUE))
  
  # esperanando antes de prosseguir
  Sys.sleep(runif(n = 1, min = 1, max = 5))
}

# função para parsear o resultado
parser_pagina <- function(path_to_html){
  
  ## lendo a pagina raspada
  pagina_raspada <- read_html(x = path_to_html)
  
  # pegando todos os links que estão dentro da tabela 
  links_da_pagina <- pagina_raspada %>% 
    xml_find_all(xpath = '//table//a[@class="primary"]') %>% 
    xml_attr(attr = 'href')
  
  ## parseando o codigo HTML da tabela para um tibble
  tabela_da_pagina <- pagina_raspada %>% 
    xml_find_all(xpath = '//table') %>% 
    rvest::html_table() %>% 
    pluck(1) %>% 
    mutate(link = links_da_pagina) %>% 
    mutate(across(everything(), as.character))
  
  ## retornando a tabela
  tabela_da_pagina
}

# obtendo paginas -------------------------------------------------------------------

## definindo qual o numero maximo de paginas para pegar
ultima_pagina <- pega_max_paginas(url_base = base_url)

## pegando as paginas
walk(.x = 1:ultima_pagina,
     .f = pega_pagina,
     url_base = base_url, save_dir = path_temp
)

# parseando as paginas --------------------------------------------------------------

## pegando o path para as paginas
path_das_paginas <- dir_ls(path = path_temp, regexp = 'html')

## colocando todas as tabelas em um dataframe so
df <- map_dfr(.x = path_das_paginas, .f = parser_pagina)

# faxinando os dados ----------------------------------------------------------------

df <- df %>% 
  # ajustando nome das colunas
  clean_names() %>% 
  # pegando somente algumas das colunas
  select(-thumbnail_image, -shop) %>% 
  # ajustando a string do titulo
  mutate(
    # removendo o excesso de espaços da string do title
    title           = str_squish(string = title),
    # pegando a apenas o titulo do jogo
    titulo          = str_extract(string = title, 
                            pattern = '(.*)(?=\\s\\(\\-?[0-9]{1,4}\\))'),
    # ano de lançamento
    ano             = str_extract(string = title, 
                                  pattern = '(?<=\\()(\\-?[0-9]{1,4})(?=\\))'),
    # descrição
    descricao       = str_extract(string = title, 
                            pattern = '(?<=\\s\\(\\-?[0-9]{1,4}\\)\\s)(.*)'),
    # extraindo o id do jogo
    id              = str_extract(string = link, 
                                  pattern = '(?<=boardgame\\/)([0-9]+)(?=\\/)'),
    # parseando numericos para numericos
    ano             = parse_number(ano),
    board_game_rank = parse_number(board_game_rank),
    geek_rating     = parse_number(geek_rating),
    avg_rating      = parse_number(avg_rating),
    num_voters      = parse_number(num_voters)
  ) %>% 
  # organizando a tabela
  relocate(
    id, titulo, ano, .after = board_game_rank
  ) %>% 
  relocate(
    title, .after = num_voters
  ) %>% 
  # renomeando as colunas
  rename(
    rank = board_game_rank, nota_bgg = geek_rating, 
    nota_usuarios = avg_rating, votos = num_voters
  )
df

# exportando os dados ---------------------------------------------------------------

write_rds(x = df, file = str_glue('{path_data}/ranking_bgg.rds'))
