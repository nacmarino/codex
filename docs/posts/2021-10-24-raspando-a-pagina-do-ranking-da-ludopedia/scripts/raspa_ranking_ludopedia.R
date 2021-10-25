
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
base_url <- 'https://www.ludopedia.com.br/ranking?pagina='

## path do post
path_post <- '_posts/2021-10-08-raspando-a-pagina-do-ranking-da-ludopedia/'

## path para diretorio temporairio para fazer o dump das paginas
path_temp <- paste0(path_post, 'temp_ludopedia')

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
    # pegando o xpath da paginacao
    xml_find_all(xpath = '//ul[@class="pagination"]//a[@title="Última Página"]') %>% 
    # pegando o link que contem o numero da pagina maxima
    xml_attr('href') %>% 
    # pegando o numero da pagina
    str_extract(pattern = '(?<=pagina=)([0-9]+)') %>% 
    # parseando para numero
    parse_number()
}

# função para fazer o GET
pega_pagina <- function(url_base, pagina, save_dir) {
  ## junta a base url com o numero da pagina e salva no diretorio alvo
  GET(url = str_glue(url_base, pagina), 
      write_disk(path = sprintf(fmt = '%s/pagina_%03d.html', save_dir, pagina), 
                 overwrite = TRUE))
  
  # esperanando antes de prosseguir
  Sys.sleep(runif(n = 1, min = 1, max = 5))
}

# função para parsear uma pagina
parser_pagina <- function(path_to_html){
  
  ## lendo a pagina raspada
  pagina_raspada <- read_html(x = path_to_html)
  
  ## infos do heading
  media_head <- pagina_raspada %>% 
    xml_find_all(xpath = '//h4[@class="media-heading"]')
  
  ## link para a imagem da capa
  links_da_capa <- pagina_raspada %>% 
    xml_find_all(xpath = '//img[@class="img-capa"]') %>% 
    xml_attr(attr = 'src')
  
  ## link para a pagina do jogo
  link_jogo <- media_head %>% 
    xml_find_all(xpath = 'a') %>% 
    xml_attr(attr = 'href')
  
  ## posicao do ranking de cada titulo
  posicao_ranking <- media_head %>% 
    xml_find_all(xpath = 'span[@class="rank"]') %>% 
    xml_text()
  
  ## nome do jogo
  titulo_jogo <- media_head %>% 
    xml_find_all(xpath = 'a[@title]') %>% 
    xml_text()
  
  ## ano de lancamento do jogo
  ano_jogo <- media_head %>% 
    xml_find_all(xpath = 'small') %>% 
    xml_text()
  
  ## informacoes gerais das notas
  notas_jogo <- pagina_raspada %>% 
    xml_find_all(xpath = '//div[@class="rank-info"]') %>% 
    xml_text()
  
  ## colocando rsultados numa tibble
  tibble(
    ranking   = posicao_ranking, 
    titulo    = titulo_jogo, 
    ano       = ano_jogo, # 
    notas     = notas_jogo,
    link_capa = links_da_capa,
    link_jogo = link_jogo
  )
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
  mutate(
    # parseando o ranking para numerico
    ranking = parse_number(ranking),
    # tratando o string titulo do jogo
    titulo  = str_squish(string = titulo),
    # parseando o ano para numerico
    ano     = parse_number(ano),
    # ajustando a string do campo de nota
    notas   = str_squish(string = notas),
  ) %>% 
  # separando a coluna com as informacoes de nota atraves do padrao da barra
  separate(col = notas, into = c('nota_rank', 'nota_media', 'notas', 'leftover'), sep = '\\|') %>% 
  # tratando as informacoes da coluna separada
  mutate(
    # nota do ranking
    nota_rank  = parse_number(nota_rank),
    # nota dos usuarios
    nota_media = parse_number(nota_media),
    # quantidade de notas
    notas      = parse_number(notas) 
  ) %>% 
  # removendo colunas que nao serao mais necessarias
  select(-leftover)

# exportando os dados ---------------------------------------------------------------

write_rds(x = df, file = str_glue('{path_data}/ranking_ludopedia.rds'))
