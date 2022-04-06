
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse) # core
library(janitor) # para ajudar a limpar os dados

# carregando os dados ---------------------------------------------------------------

## carregando os dados as is
dados <- read_csv(file = '_posts/2022-12-31-state-of-data-2021/data/State of Data 2021 - Dataset - Pgina1.csv')

# criando um dicionario de dados ----------------------------------------------------

## colocando o nome das colunas do dataframe original dentro de um tibble
dicionario <- tibble(
  coluna = names(dados) 
) %>%
  ## limpando o string com o nome das colunas
  mutate(
    # removendo a aspas simples do nome da coluna
    informacao = str_replace_all(string = coluna, pattern = "'", replacement = ''),
    # removendo os parenteses do nome das colunas
    informacao = str_replace_all(string = informacao, pattern = '\\(|\\)', replacement = ''),
    # ajustando a coluna do id da pessoa para ficar mais facil de quebrar essa informacao depois 
    # em multiplas colunas com base no padrao espaco-virgula-espaco
    informacao = str_replace_all(string = informacao, pattern = 'P0, id', replacement = 'P0 , id')
  ) %>% 
  # separando o id da pergunta do string de seu texto
  separate(col = 'informacao', into = c('pergunta', 'texto'), sep = ' , ') %>% 
  # separando o id da pergunta em parte, letra da pergunta e letra da opcao escolhida
  separate(col = 'pergunta', into = c('parte', 'pergunta', 'opcao'), sep = '_') %>% 
  # encodando a coluna que contém a pergunta principal - o regex abaixo remove todas as colunas que
  # representam o one hot encoding das opcoes dadas aos usuarios
  mutate(
    pergunta_principal = str_detect(string = coluna, pattern = 'P[2-9]_[a-z]_', negate = TRUE)
  )
dicionario

# extraindo as informacoes que vamos trabalhar --------------------------------------

## filtrando as colunas que contém as perguntas principais da enquete
target_infos <- dicionario %>% 
  # filtrando so as perguntas principais da enquente
  filter(pergunta_principal)

## criando a base que vamos trabalhar
df <- dados %>% 
  # pegando so as colunas que vamos trabalhar
  select(pull(target_infos, coluna)) %>% 
  # renomeando as colunas
  set_names(nm = pull(target_infos, texto)) %>% 
  # limpando os nomes
  clean_names()
df
