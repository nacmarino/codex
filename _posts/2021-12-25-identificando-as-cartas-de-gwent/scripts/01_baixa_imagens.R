
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

library(tidyverse)
library(fs)

# criando funcoes -------------------------------------------------------------------

## funcao para baixar uma imagem a partir da url
pega_imagem <- function(image_path, identificador, path_to_save) {
  # setando a url onde esta a imagem
  target_url <- str_glue('https://www.playgwent.com/{image_path}')
  # setando o nome que usaremos para salvar o arquivo local
  nome_da_imagem <- str_extract(string = image_path, pattern = '(thumb_[0-9]+)')
  # fazendo o download da imagem
  download.file(url = target_url, destfile = str_glue('{path_to_save}/{identificador}_{nome_da_imagem}.jpeg'))
  # aguardando antes de seguir
  Sys.sleep(runif(n = 1, min = 1, max = 2))
}

# carregando os dados ---------------------------------------------------------------

dados <- read_rds(file = '_posts/2021-12-25-quais-as-associacoes-existentes-entre-as-cartas-dos-decks-de-gwent/data/decks.rds')
dados

# pegando as cartas distintas -------------------------------------------------------

## pegando as cartas unicas e extraindo o id delas
cartas <- dados %>% 
  select(id, name, big) %>% 
  distinct()

# criando pastas --------------------------------------------------------------------

## definindo os paths que serao criados
### para as imagens grandes
big_path <- '_posts/2021-12-25-identificando-as-cartas-de-gwent/data'

## criando pasta temporaria caso necessario
if(!dir_exists(big_path)){
  dir_create(path = big_path, recurse = TRUE)
}

# baixando as imagens ---------------------------------------------------------------

walk2(.x = cartas$big, .y = cartas$id, .f = pega_imagem, path_to_save = big_path)
