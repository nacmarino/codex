
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(httr)

# pegando as funcoes --------------------------------------------------------------------------------------------------------------------------------------

source(file = 'R/autenticacao.R')
source(file = 'R/pega_info_jogo.R')

# pegando os dados da lista de jogos ----------------------------------------------------------------------------------------------------------------------

## lista consolidada com todos os jogos
jogos <- read_rds(file = 'data/tidy/lista_de_jogos.rds')

# autenticando a sessao -----------------------------------------------------------------------------------------------------------------------------------

## pegando o token de acesso
access_token <- pegar_access_token(app = 'BoardGameApp', app_id = '72944eeeab6905f1', uris = 'http://localhost:1410/')

# pegando as informacoes de cada jogo ---------------------------------------------------------------------------------------------------------------------

# walk(.x = jogos$id_jogo,
#      .f = ~ informacao_do_jogo(id_jogo = .x, access_token = access_token, retornar_df = FALSE, salvar = TRUE, path_salvar = 'data/raw/infos_dos_jogos/')
# )

# lendo os dataframes com as caracteristicas de cada jogo -------------------------------------------------------------------------------------------------

## pegando todos os arquivos
infos <- list.files(path = 'data/raw/infos_dos_jogos/', pattern = 'info_do_jogo_') %>% 
  map_dfr(.f = ~ read_rds(file = str_glue('data/raw/infos_dos_jogos/', .))) %>% 
  drop_na(id_jogo) %>% 
  distinct()

# desempacotando a lista de mecanicas ---------------------------------------------------------------------------------------------------------------------

## desempacotando a lista de mecanicas
mecanicas <- infos %>% 
  select(id_jogo, mecanicas) %>% 
  drop_na() %>% 
  unnest(cols = mecanicas) %>% 
  distinct()

# desempacotando a lista de categorias --------------------------------------------------------------------------------------------------------------------

## desempacotando a lista de categorias
categorias <- infos %>% 
  select(id_jogo, categorias) %>% 
  drop_na() %>% 
  unnest(cols = categorias) %>% 
  distinct()

# desempacotando a lista de temas -------------------------------------------------------------------------------------------------------------------------

## desempacotando a lista de temas
temas <- infos %>% 
  select(id_jogo, temas) %>% 
  drop_na() %>% 
  unnest(cols = temas) %>% 
  distinct()

# desempacotando a lista de artistas -----------------------------------------------------------------------------------------------------------------------

## desempacotando a lista de artistas
artistas <- infos %>% 
  select(id_jogo, artistas) %>% 
  drop_na() %>% 
  unnest(cols = artistas) %>% 
  distinct()

# desempacotando a lista de designers ----------------------------------------------------------------------------------------------------------------------

## desempacotando a lista de designers
designers <- infos %>% 
  select(id_jogo, designers) %>% 
  drop_na() %>% 
  unnest(cols = designers) %>% 
  distinct()

# juntando e organizando as informacoes -------------------------------------------------------------------------------------------------------------------

infos <- infos %>% 
  # removendo as colunas lista
  select(-mecanicas, -categorias, -temas, -artistas, -designers) %>% 
  # codificando novas informacoes
  mutate(
    # ajustando o texto da coluna de tipo de jogo
    tp_jogo = case_when(tp_jogo == 'b' ~ 'base',
                        tp_jogo == 'e' ~ 'expansao',
                        TRUE ~ 'desconhecido'),
    # adicionando dummy de jogo disponivel no brasil ou nao 
    disponivel_br = ifelse(test = is.na(ano_nacional), yes = 'nao', no = 'sim')
  ) %>% 
  # juntando informacoes das mecanicas de cada jogo
  left_join(y = mecanicas %>% 
              group_by(id_jogo) %>% 
              summarise(mecanicas = paste0(nm_mecanica, collapse = ';')),
            by = 'id_jogo') %>% 
  # juntando informacoes das categorias de cada jogo
  left_join(y = categorias %>% 
              group_by(id_jogo) %>% 
              summarise(categorias = paste0(nm_categoria, collapse = ';')),
            by = 'id_jogo') %>% 
  # juntando informacoes dos temas de cada jogo
  left_join(y = temas %>% 
              group_by(id_jogo) %>% 
              summarise(temas = paste0(nm_tema, collapse = ';')),
            by = 'id_jogo') %>% 
  # juntando informacoes do artista de cada jogo
  left_join(y = artistas %>% 
              group_by(id_jogo) %>% 
              summarise(artistas = paste0(nm_profissional, collapse = ';')),
            by = 'id_jogo') %>% 
  # juntando informacoes do designer do jogo
  left_join(y = designers %>% 
              group_by(id_jogo) %>% 
              summarise(designers = paste0(nm_profissional, collapse = ';')),
            by = 'id_jogo') %>% 
  # passando variaveis numericas para numerica
  mutate_at(.vars = vars(ano_publicacao:qt_jogou), as.numeric) %>% 
  # preenchendo valores faltantes por zero nas respectivas colunas
  replace_na(replace = list(qt_tem = 0, qt_teve = 0, qt_favorito = 0, qt_quer = 0, qt_jogou = 0))

# salvando os dados ---------------------------------------------------------------------------------------------------------------------------------------

write_rds(x = infos, file = 'data/tidy/infos_dos_jogos.rds')
write_rds(x = mecanicas, file = 'data/tidy/mecanicas.rds')
write_rds(x = categorias, file = 'data/tidy/categorias.rds')
write_rds(x = temas, file = 'data/tidy/temas.rds')
write_rds(x = artistas, file = 'data/tidy/artistas.rds')
write_rds(x = designers, file = 'data/tidy/designers.rds')
