
# pergunta-alvo ---------------------------------------------------------------------

## o que diferencia uma pessoa de dados senior, pleno e junior?

# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse) # core
library(janitor) # para ajudar a limpar os dados
library(vegan) # para analise
library(betapart) # para decomposicao da diversidade beta

# carregando os dados ---------------------------------------------------------------

## carregando os dados as is
dados <- read_csv(file = '_posts/2022-12-31-state-of-data-2021/data/raw/State of Data 2021 - Dataset - Pgina1.csv')

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
  separate(col = 'informacao', into = c('pergunta_id', 'texto'), sep = ' , ') %>% 
  # separando o id da pergunta em parte, letra da pergunta e letra da opcao escolhida
  separate(col = 'pergunta_id', into = c('parte', 'pergunta', 'opcao'), sep = '_', remove = FALSE) %>% 
  # fazendo mais alguns ajustes a base
  mutate(
    # encodando a coluna que contém a pergunta principal - o regex abaixo remove todas as colunas que
    # representam o one hot encoding das opcoes dadas aos usuarios
    pergunta_principal = str_detect(string = coluna, pattern = 'P[2-9]_[a-z]_', negate = TRUE),
    # ajustando o problema de separacao do identificador das colunas da parte 3, pergunta d
    opcao = case_when(pergunta_id == 'P3_d_' ~ str_extract(string = texto, pattern = '^[a-k](?=\\s)'),
                      TRUE ~ opcao),
    texto = case_when(pergunta_id == 'P3_d_' ~ str_remove(string = texto, pattern = '^[a-k]\\s'),
                      TRUE ~ texto),
    pergunta_id = case_when(pergunta_id == 'P3_d_' ~ paste0(pergunta_id, opcao),
                            TRUE ~ pergunta_id)
  )
dicionario

# ajustando a base de dados e derivando novas features ------------------------------

## colocando nomes das colunas como o codigo da parte, pergunta e opcao
dados <- dados %>% 
  set_names(nm = pull(dicionario, pergunta_id))

## criando features novas ao nivel do id
dados %>% 
  transmute(
    # pegando o id de cada respondente de volta
    P0      = P0,
    # recodificando a informação de idade de forma similar àquela do relatorio
    idade   = case_when(
      P1_a_a %in% c('22-24', '25-29') ~ '22-29',
      P1_a_a %in% c('30-34', '35-39') ~ '30-39',
      P1_a_a %in% c('40-44', '45-49') ~ '40-49',
      P1_a_a %in% c('50-54', '55+') ~ '>= 50',
      TRUE ~ P1_a_a
    ),
    # recodificando a informação do salario de forma similar àquela do relatorio
    salario = case_when(
      P2_h %in% c('Menos de R$ 1.000/mês', 'de R$ 1.001/mês a R$ 2.000/mês') ~ '<= R$ 2.000 mês',
      P2_h %in% c('de R$ 2.001/mês a R$ 3000/mês', 'de R$ 3.001/mês a R$ 4.000/mês') ~ 'R$ 2k/mês - R$ 4k/mês',
      P2_h %in% c('de R$ 4.001/mês a R$ 6.000/mês') ~ 'R$ 4k/mês - R$ 6k/mês',
      P2_h %in% c('de R$ 6.001/mês a R$ 8.000/mês') ~ 'R$ 6k/mês - R$ 8k/mês',
      P2_h %in% c('de R$ 8.001/mês a R$ 12.000/mês') ~ 'R$ 8k/mês - R$ 12k/mês',
      P2_h %in% c('de R$ 12.001/mês a R$ 16.000/mês') ~ 'R$ 12k/mês - R$ 16k/mês',
      P2_h %in% c('de R$ 16.001/mês a R$ 20.000/mês', 'de R$ 20.001/mês a R$ 25.000/mês', 'de R$ 25.001/mês a R$ 30.000/mês', 'de R$ 30.001/mês a R$ 40.000/mês', 'Acima de R$ 40.001/mês') ~ '> R$ 16k/mês'
    ),
    # codificando se a pessoa que atua como analista de dados possui esse titulo como cargo ou nao
    role_AD = case_when(
      P4_a != 'Análise de Dados' ~ 'not_applicable',
      P4_a == 'Análise de Dados' & str_detect(string = P2_f, pattern = 'Analista de Dados|Analista de BI') ~ 'has_title',
      P4_a == 'Análise de Dados' & str_detect(string = P2_f, pattern = 'Analista de Dados|Analista de BI', negate = TRUE) ~ 'no_title',
      TRUE ~ 'unknown'
    ),
    # codificando se a pessoa que atua como cientista de dados possui esse titulo como cargo ou nao
    role_DS = case_when(
      P4_a != 'Ciência de Dados' ~ 'not_applicable',
      P4_a == 'Ciência de Dados' & str_detect(string = P2_f, pattern = 'Cientista de Dados') ~ 'has_title',
      P4_a == 'Ciência de Dados' & str_detect(string = P2_f, pattern = 'Cientista de Dados', negate = TRUE) ~ 'no_title',
      TRUE ~ 'unknown'
    ),
    # codificando se a pessoa que atua como engenheiro de dados possui esse titulo como cargo ou nao
    role_ED = case_when(
      P4_a != 'Engenharia de Dados' ~ 'not_applicable',
      P4_a == 'Engenharia de Dados' & str_detect(string = P2_f, pattern = 'Engenheiro de Dados') ~ 'has_title',
      P4_a == 'Engenharia de Dados' & str_detect(string = P2_f, pattern = 'Engenheiro de Dados', negate = TRUE) ~ 'no_title',
      TRUE ~ 'unknown'
    )
  )

# colunas alvo ----------------------------------------------------------------------

# P4: atuacao geral (b à h)
# P6: engenheiro de dados
# P7: análise de dados
# P8: ciencia de dados
  
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

# cientista de dados diferem entre os niveis? -------------------------------------------------------------------------------------------------------------

## preparando os dados dos cientistas de dados por nivel
df_ds <- dados %>%
  # ajustando os nomes das colunas para um padrao
  set_names(nm = pull(dicionario, pergunta_id)) %>% 
  # limpando o nome das colunas
  clean_names() %>% 
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(p2_d == 0, !is.na(p8_a) | !is.na(p8_b) | !is.na(p8_c) | !is.na(p8_d)) %>% 
  # # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(p0, p2_g, contains('p8_a_'), contains('p8_b_'), contains('p8_c_'), contains('p8_d_'))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_ds, contains('p8')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_DS <- beta.pair(select(df_ds, contains('p8')), index.family = 'jaccard')

## ajustando um PERMDISP aos dados
beta_disper_DS <- betadisper(pluck(beta_part_DS, 3), group = df_ds$p2_g)
## visualizando o resultado da PERMDISP
plot(beta_disper_DS)
## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_DS, 3) ~ p2_g, data = df_ds)

# engenheiros de dados diferem entre os niveis? -----------------------------------------------------------------------------------------------------------

## preparando os dados dos engenheiros de dados por nivel
df_en <- dados %>%
  # ajustando os nomes das colunas para um padrao
  set_names(nm = pull(dicionario, pergunta_id)) %>% 
  # limpando o nome das colunas
  clean_names() %>% 
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(p2_d == 0, !is.na(p6_a) | !is.na(p6_b)) %>% 
  # # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(p0, p2_g, contains('p6_a_'), contains('p6_b_'))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_en, contains('p6')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_EN <- beta.pair(select(df_en, contains('p6')), index.family = 'jaccard')

## ajustando um PERMDISP aos dados
beta_disper_EN <- betadisper(pluck(beta_part_EN, 3), group = df_en$p2_g)
## visualizando o resultado da PERMDISP
plot(beta_disper_EN)
## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_EN, 3) ~ p2_g, data = df_en)

# analistas de dados diferem entre os niveis? -------------------------------------------------------------------------------------------------------------

## preparando os dados dos engenheiros de dados por nivel
df_ad <- dados %>%
  # ajustando os nomes das colunas para um padrao
  set_names(nm = pull(dicionario, pergunta_id)) %>% 
  # limpando o nome das colunas
  clean_names() %>% 
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(p2_d == 0, !is.na(p7_a) | !is.na(p7_b) | !is.na(p7_d)) %>% 
  # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(p0, p2_g, contains('p7_a_'), contains('p7_b_'), contains('p7_d_')) %>% 
  # 
  drop_na()

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_ad, contains('p7')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_AD <- beta.pair(select(df_ad, contains('p7')), index.family = 'jaccard')

## ajustando um PERMDISP aos dados
beta_disper_AD <- betadisper(pluck(beta_part_AD, 3), group = df_ad$p2_g)
## visualizando o resultado da PERMDISP
plot(beta_disper_AD)
## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_AD, 3) ~ p2_g, data = df_ad)
