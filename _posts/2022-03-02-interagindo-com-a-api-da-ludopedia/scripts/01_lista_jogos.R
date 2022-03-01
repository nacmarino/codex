
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(httr)

# pegando as funcoes --------------------------------------------------------------------------------------------------------------------------------------

source(file = 'R/autenticacao.R')
source(file = 'R/pega_jogos.R')

# autenticando a sessao -----------------------------------------------------------------------------------------------------------------------------------

## pegando o token de acesso
access_token <- pegar_access_token(app = 'BoardGameApp', app_id = '72944eeeab6905f1', uris = 'http://localhost:1410/')

# pegando o total de jogos cadastrados --------------------------------------------------------------------------------------------------------------------

## quantidade total de jogos cadastrados
n_jogos <- total_jogos(access_token = access_token)

# pegando a lista de jogos cadastrados --------------------------------------------------------------------------------------------------------------------

walk(.x = 1:ceiling(n_jogos / 100), 
     .f = ~ pega_pagina(pagina = .x, 
                        access_token = access_token, 
                        retornar_df = FALSE, 
                        salvar = TRUE, 
                        path_salvar = 'data/raw/lista_de_jogos/')
)

# lendo a lista de jogos cadastrados salva ----------------------------------------------------------------------------------------------------------------

## pegando todos os arquivos salvos
lista_de_jogos <- list.files(path = 'data/raw/lista_de_jogos/', pattern = 'lista_jogos_') %>% 
  map_dfr(.f = ~ read_rds(str_glue('data/raw/lista_de_jogos/', .))
  ) %>% 
  mutate(id_jogo = as.numeric(id_jogo)) %>% 
  arrange(id_jogo)

# salvando a lista de jogos -------------------------------------------------------------------------------------------------------------------------------

write_rds(x = lista_de_jogos, file = 'data/tidy/lista_de_jogos.rds')
