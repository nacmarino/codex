
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(httr)

# pegando as funcoes --------------------------------------------------------------------------------------------------------------------------------------

source(file = 'R/autenticacao.R')
source(file = 'R/pega_usuarios.R')

# autenticando a sessao -----------------------------------------------------------------------------------------------------------------------------------

## pegando o token de acesso
access_token <- pegar_access_token(app = 'BoardGameApp', app_id = '72944eeeab6905f1', uris = 'http://localhost:1410/')

# pegando o total de usuarios -----------------------------------------------------------------------------------------------------------------------------

n_usuarios <- total_usuarios(access_token = access_token)

# pegando a lista de usuarios -----------------------------------------------------------------------------------------------------------------------------

# walk(.x = 1:ceiling(n_usuarios / 100),
#      .f = ~ lista_usuarios(pagina = .x, access_token = access_token, retornar_df = FALSE, salvar = TRUE, path_salvar = 'data/raw/lista_de_usuarios/')
# )

# consolidando a lista de usuarios ------------------------------------------------------------------------------------------------------------------------

# id dos usuarios
usuarios <- list.files(path = 'data/raw/lista_de_usuarios/', pattern = 'lista_usuarios_') %>% 
  map_dfr(.f = ~ read_rds(file = paste0('data/raw/lista_de_usuarios/', .)))

# catando quantidade de titulos de cada usuario -----------------------------------------------------------------------------------------------------------

# walk(.x = usuarios$id_usuario,
#      .f = ~ total_titulos_usuarios(id_usuario = .x, access_token = access_token, retornar_df = FALSE, salvar = TRUE, path_salvar = 'data/raw/tamanho_colecoes/')
# )

# tratando base dos usuarios ------------------------------------------------------------------------------------------------------------------------------

# organizando base dos usuarios extraidos
usuarios <- usuarios %>% 
  mutate(usuario = str_trim(string = usuario)) %>% 
  arrange(id_usuario)

# lendo a base com a quantidade de titulos por usuarios
n_titulos <- list.files(path = 'data/raw/tamanho_colecoes/', pattern = 'tamanho_colecao_usuario_') %>% 
  map_dfr(.f = ~ read_rds(file = paste0('data/raw/tamanho_colecoes/', .)))

# definindo quais usuarios buscaremos 
usuarios_para_buscar <- n_titulos %>% 
  select(id_usuario, colecao:teve, jogados) %>% 
  pivot_longer(names_to = 'lista', values_to = 'titulos', cols = -id_usuario) %>% 
  filter(titulos > 0) %>% 
  mutate(paginas = ceiling(x = titulos / 100))

# pegando as colecoes -------------------------------------------------------------------------------------------------------------------------------------

## pegando os usuarios que serão buscados
usuarios_colecao <- filter(usuarios_para_buscar, lista == 'colecao') %>% 
  arrange(id_usuario)

## loopando entre as colecoes de cada usuario
# walk2(.x = usuarios_colecao$id_usuario,
#       .y = usuarios_colecao$paginas, 
#       .f = ~ {
#         pega_varias_paginas(id_usuario = .x, lista = 'colecao', n_paginas = .y, access_token = access_token) %>% 
#           write_rds(file = str_glue('data/raw/lista_de_colecoes/colecao_usuario_{.x}.rds'))
#       }
# )

## lendo todas as tabelas baixadas
colecoes <- list.files(path = 'data/raw/lista_de_colecoes/', pattern = 'colecao_usuario_') %>% 
  map_dfr(.f = ~ read_rds(file = paste0('data/raw/lista_de_colecoes/', .)))

# exportando os dados das colecoes ------------------------------------------------------------------------------------------------------------------------

write_rds(x = usuarios, file = 'data/tidy/lista_de_usuarios.rds')
write_rds(x = n_titulos, file = 'data/tidy/n_titulos_por_usuario.rds')
write_rds(x = colecoes, file = 'data/tidy/colecoes_dos_usuarios.rds')
