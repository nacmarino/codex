
# pergunta-alvo ---------------------------------------------------------------------

## o que diferencia uma pessoa de dados senior, pleno e junior?

# nao esquecer ----------------------------------------------------------------------------------------------------

## existem IDs duplicados

# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

install# carregando pacotes ----------------------------------------------------------------

library(tidyverse) # core
library(janitor) # para ajudar a limpar os dados
library(readxl) # para ler excel
library(vegan) # para analise
library(betapart) # para decomposicao da diversidade beta
library(factoextra) # para clusterizacao
library(arules) # para regras de associacao
library(corrr) # correlacoes tidy
library(yardstick) # metricas de avaliacao
library(heatmaply) # heatmaps com plotly
library(ggraph) # para grafos
library(tidymodels) # modelagem
library(furrr) # processamento paralelo
library(finetune) # ajudar na otimizacao de hiperparametros
library(cluster) # gower e pam
library(Rtsne) # tsne
library(tidytext) # para ajudar com texto
library(dials) # hiperparametros
library(DALEX) # explicabilidade
library(DALEXtra) # explicabilidade + tidymodels

# carregando os dados ---------------------------------------------------------------

## carregando os dados as is
dados <- read_csv(file = '_posts/2022-12-31-state-of-data-2021/data/raw/State of Data 2021 - Dataset - Pgina1.csv')

## carregando os contra-factuais
contrafactuais <- read_excel(path = '_posts/2022-12-31-state-of-data-2021/data/outputs/counterfactuals.xlsx')

## carregando os coeficientes do sklearn
coeficients_sklearn <- read_excel(path = '_posts/2022-12-31-state-of-data-2021/data/outputs/sklearn_coefficients.xlsx')

# ajustando o nome das colunas atraves de um dicionario de dados --------------------

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

## colocando nomes das colunas como o codigo da parte, pergunta e opcao
dados <- dados %>% 
  set_names(nm = pull(dicionario, pergunta_id)) %>% 
  # corrigindo o nome do arquiteto de dados
  mutate(
    P2_f = ifelse(test = P2_f == 'Arquiteto de dados', yes = 'Arquiteto de Dados', no = P2_f)
  )

# derivando features a partir dos dados ---------------------------------------------

## criando features novas ao nivel do id
df_features <- dados %>% 
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
    role = case_when(
      P2_d == 1 | is.na(P2_d) | P4_a == 'Outra' ~ 'not_applicable',
      P4_a == 'Análise de Dados' & str_detect(string = P2_f, pattern = 'Analista de Dados|Analista de BI') ~ 'has_role',
      P4_a == 'Ciência de Dados' & str_detect(string = P2_f, pattern = 'Cientista de Dados') ~ 'has_role',
      P4_a == 'Engenharia de Dados' & str_detect(string = P2_f, pattern = 'Engenheiro de Dados') ~ 'has_role',
      TRUE ~ 'no_role'
    ),
    # codificando se a pessoa que atua como analista de dados possui esse titulo como cargo ou nao
    role_AD = case_when(
      P4_a != 'Análise de Dados' ~ 'not_applicable',
      P4_a == 'Análise de Dados' & str_detect(string = P2_f, pattern = 'Analista de Dados') ~ 'has_role',
      P4_a == 'Análise de Dados' & str_detect(string = P2_f, pattern = 'Analista de Dados', negate = TRUE) ~ 'no_role',
      TRUE ~ 'unknown'
    ),
    # codificando se a pessoa que atua como cientista de dados possui esse titulo como cargo ou nao
    role_DS = case_when(
      P4_a != 'Ciência de Dados' ~ 'not_applicable',
      P4_a == 'Ciência de Dados' & str_detect(string = P2_f, pattern = 'Cientista de Dados') ~ 'has_role',
      P4_a == 'Ciência de Dados' & str_detect(string = P2_f, pattern = 'Cientista de Dados', negate = TRUE) ~ 'no_role',
      TRUE ~ 'unknown'
    ),
    # codificando se a pessoa que atua como engenheiro de dados possui esse titulo como cargo ou nao
    role_ED = case_when(
      P4_a != 'Engenharia de Dados' ~ 'not_applicable',
      P4_a == 'Engenharia de Dados' & str_detect(string = P2_f, pattern = 'Engenheiro de Dados') ~ 'has_role',
      P4_a == 'Engenharia de Dados' & str_detect(string = P2_f, pattern = 'Engenheiro de Dados', negate = TRUE) ~ 'no_role',
      TRUE ~ 'unknown'
    ),
    tamanho_empresa = case_when(
      is.na(P2_c) ~ 'Desconhecido',
      P2_c %in% c('de 1 a 5', 'de 6 a 10') ~ 'Microempresa',
      P2_c %in% c('de 11 a 50') ~ 'Pequeno porte',
      P2_c %in% c('de 51 a 100') ~ 'Médio porte',
      TRUE ~ 'Grande porte'
    ),
    area_dados = case_when(
      is.na(P3_a) ~ 'Desconhecido',
      P3_a == 'Ainda não temos pessoas atuando com dados na empresa' ~ 'Não há',
      P3_a %in% c('1 - 3', '4 - 10') ~ 'Pequena',
      P3_a %in% c('11 - 20', '21 - 50') ~ 'Média',
      TRUE ~ 'Grande'
    ),
    tempo_experiencia = case_when(
      is.na(P2_i) ~ 'Desconhecido',
      P2_i %in% c('de 1 a 2 anos', 'de 2 a 3 anos') ~ 'de 1 a 3 anos',
      P2_i %in% c('de 4 a 5 anos', 'de 6 a 10 anos') ~ 'de 4 a 10 anos',
      TRUE ~ P2_i
    ),
    target = case_when(
      P4_a %in% c('Gestor', 'Outra') ~ 'Fora de escopo',
      TRUE ~ paste0(P4_a, ' - ', P2_g)
    ),
    instrucao = case_when(
      P1_h %in% c('Estudante de Graduação', 'Não tenho graduação formal', 'Prefiro não informar') ~ 'Sem diploma',
      TRUE ~ P1_h
    )
  )
df_features

# ATUAÇÃO GERAL ---------------------------------------------------------------------

## preparando os dados sobre a atuacao geral das pessoas
df_general <- dados %>% 
  # removendo tudo o que for dado de gestor e outras atuacoes
  filter(P2_d == 0, P4_a != 'Outra') %>% 
  # selecionando apenas as colunas com as informacoes de atuacao geral que usaremos
  select(P0, P4_a, P2_g, contains('P4_b_'), contains('P4_d_'), contains('P4_f_'), contains('P4_g_'), contains('P4_h_')) %>% 
  select(-P4_h_x) %>% 
  # juntando com featuresx
  left_join(y = df_features, by = 'P0') %>% 
  # dropando qualquer na
  drop_na()

## temperatura da matriz
temp_GEN <- nestedtemp(comm = select(df_general, P4_b_a:P4_h_v))
plot(temp_GEN, kind = 'incidence', names = c(FALSE, TRUE))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_general, P4_b_a:P4_h_v), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_GEN <- beta.pair(select(df_general, P4_b_a:P4_h_v), index.family = 'jaccard')

## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_GEN, 3) ~ P4_a * P2_g * role, data = df_general)

## ajustando um PERMDISP aos dados
beta_disper_GEN <- betadisper(pluck(beta_part_GEN, 3), group = interaction(df_general$role, df_general$P2_g, df_general$P4_a), type = 'centroid')
## visualizando o resultado da PERMDISP
plot(beta_disper_GEN)
betadisper(pluck(beta_part_GEN, 3), group = df_general$role, type = 'centroid')
betadisper(pluck(beta_part_GEN, 3), group = df_general$P2_g, type = 'centroid')
betadisper(pluck(beta_part_GEN, 3), group = df_general$P4_a, type = 'centroid')

## olhando a dispersao por grupos
beta_disper_GEN
## testando a dispersao por grupos
permutest(beta_disper_GEN, pairwise = TRUE)

## valor de kappa entre as respostas para as perguntas
df_general_kappa <- df_general %>% 
  mutate(
    is_senior = as.double(P2_g == 'Sênior'), is_pleno = as.double(P2_g == 'Pleno'), is_junior = as.double(P2_g == 'Júnior')
  ) %>% 
  select(P4_b_a:P4_h_v, contains('is_')) %>% 
  mutate(across(everything(), factor, levels = c(0, 1))) %>% 
  colpair_map(.f = kap_vec)

## visualizando o valor de kappa entre todas as respostas
df_general_kappa %>% 
  data.frame %>% 
  `rownames<-`(value = .$term) %>% 
  select(-term) %>% 
  heatmaply()

## quais sao as respostas mais correlacionadas?
df_general_kappa %>% 
  shave() %>% 
  stretch() %>% 
  drop_na() %>% filter(y == 'is_senior') %>% arrange(-r)
filter(abs(r) >= 0.25) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('x' = 'pergunta_id')) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('y' = 'pergunta_id')) %>% 
  mutate(
    texto.x = ifelse(test = is.na(texto.x), yes = x, no = texto.x),
    texto.y = ifelse(test = is.na(texto.y), yes = y, no = texto.y),
  ) %>% 
  select(texto.x, texto.y, r) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_colour = r), arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_color_distiller(palette = 'RdYlBu', direction = 1) +
  theme_void()

# ATUAÇÃO ESPECÍFICA: CIENTISTA DE DADOS --------------------------------------------

## preparando os dados dos cientistas de dados por nivel
df_ds <- dados %>%
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(P2_d == 0, !is.na(P8_a) | !is.na(P8_b) | !is.na(P8_c) | !is.na(P8_d)) %>% 
  # # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(P0, P2_g, contains('P8_a_'), contains('P8_b_'), contains('P8_c_'), contains('P8_d_')) %>% 
  # juntando com features
  left_join(y = df_features, by = 'P0')

## temperatura da matriz
temp_DS <- nestedtemp(comm = select(df_ds, contains('P8')))
plot(temp_DS, kind = 'incidence', names = c(FALSE, TRUE))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_ds, contains('P8')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_DS <- beta.pair(select(df_ds, contains('P8')), index.family = 'jaccard')

## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_DS, 3) ~ P2_g * role, data = df_ds)

## ajustando um PERMDISP aos dados
beta_disper_DS <- betadisper(pluck(beta_part_DS, 3), group = interaction(df_ds$role, df_ds$P2_g), type = 'centroid')
beta_disper_DS
permutest(beta_disper_DS, pairwise = TRUE)
## visualizando o resultado da PERMDISP
plot(beta_disper_DS)

## valor de kappa entre as respostas para as perguntas
df_ds_kappa <- df_ds %>% 
  mutate(
    is_senior = as.double(P2_g == 'Sênior'), is_pleno = as.double(P2_g == 'Pleno'), is_junior = as.double(P2_g == 'Júnior')
  ) %>% 
  select(contains('P8'), contains('is_')) %>% 
  mutate(across(everything(), factor, levels = c(0, 1))) %>% 
  colpair_map(.f = kap_vec)

## visualizando o valor de kappa entre todas as respostas
df_ds_kappa %>% 
  data.frame %>% 
  `rownames<-`(value = .$term) %>% 
  select(-term) %>% 
  heatmaply()

## quais sao as respostas mais correlacionadas?
df_ds_kappa %>% 
  shave() %>% 
  stretch() %>% 
  drop_na() %>% 
  filter(abs(r) >= 0.25) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('x' = 'pergunta_id')) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('y' = 'pergunta_id')) %>% 
  mutate(
    texto.x = ifelse(test = is.na(texto.x), yes = x, no = texto.x),
    texto.y = ifelse(test = is.na(texto.y), yes = y, no = texto.y),
  ) %>% 
  select(texto.x, texto.y, r) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_colour = r), arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_color_distiller(palette = 'RdYlBu', direction = 1) +
  theme_void()

# ATUAÇÃO ESPECÍFICA: ENGENHEIRO DE DADOS -------------------------------------------

## preparando os dados dos engenheiros de dados por nivel
df_en <- dados %>%
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(P2_d == 0, !is.na(P6_a) | !is.na(P6_b)) %>% 
  # # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(P0, P2_g, contains('P6_a_'), contains('P6_b_')) %>% 
  # juntando com features
  left_join(y = df_features, by = 'P0')

## temperatura da matriz
temp_EN <- nestedtemp(comm = select(df_en, contains('P6')))
plot(temp_EN, kind = 'incidence', names = c(FALSE, TRUE))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_en, contains('P6')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_EN <- beta.pair(select(df_en, contains('P6')), index.family = 'jaccard')

## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_EN, 3) ~ P2_g * role, data = df_en)

## ajustando um PERMDISP aos dados
beta_disper_EN <- betadisper(pluck(beta_part_EN, 3), group = interaction(df_en$role, df_en$P2_g), type = 'centroid')
beta_disper_EN
permutest(beta_disper_EN, pairwise = TRUE)
## visualizando o resultado da PERMDISP
plot(beta_disper_EN)

## valor de kappa entre as respostas para as perguntas
df_en_kappa <- df_en %>% 
  mutate(
    is_senior = as.double(P2_g == 'Sênior'), is_pleno = as.double(P2_g == 'Pleno'), is_junior = as.double(P2_g == 'Júnior')
  ) %>% 
  select(contains('P6'), contains('is_')) %>% 
  mutate(across(everything(), factor, levels = c(0, 1))) %>% 
  colpair_map(.f = kap_vec)

## visualizando o valor de kappa entre todas as respostas
df_en_kappa %>% 
  data.frame %>% 
  `rownames<-`(value = .$term) %>% 
  select(-term) %>% 
  heatmaply()

## quais sao as respostas mais correlacionadas?
df_en_kappa %>% 
  shave() %>% 
  stretch() %>% 
  drop_na() %>% 
  filter(abs(r) >= 0.25) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('x' = 'pergunta_id')) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('y' = 'pergunta_id')) %>% 
  mutate(
    texto.x = ifelse(test = is.na(texto.x), yes = x, no = texto.x),
    texto.y = ifelse(test = is.na(texto.y), yes = y, no = texto.y),
  ) %>% 
  select(texto.x, texto.y, r) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_colour = r), arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_color_distiller(palette = 'Blues', direction = 1) +
  theme_void()

# ATUAÇÃO ESPECÍFICA: ANALISTA DE DADOS ---------------------------------------------

## preparando os dados dos engenheiros de dados por nivel
df_ad <- dados %>%
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(P2_d == 0, !is.na(P7_a) | !is.na(P7_b) | !is.na(P7_d)) %>% 
  # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(P0, P2_g, contains('P7_a_'), contains('P7_b_'), contains('P7_d_')) %>% 
  # dropando possiveis NAs
  drop_na() %>% 
  # juntando com features
  left_join(y = df_features, by = 'P0')

## temperatura da matriz
temp_AD <- nestedtemp(comm = select(df_ad, contains('P7')))
plot(temp_AD, kind = 'incidence', names = c(FALSE, TRUE))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_ad, contains('P7')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_AD <- beta.pair(select(df_ad, contains('P7')), index.family = 'jaccard')

## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_AD, 3) ~ P2_g * role, data = df_ad)

## ajustando um PERMDISP aos dados
beta_disper_AD <- betadisper(pluck(beta_part_AD, 3), group = interaction(df_ad$role, df_ad$P2_g), type = 'centroid')
beta_disper_AD
permutest(beta_disper_AD, pairwise = TRUE)
## visualizando o resultado da PERMDISP
plot(beta_disper_AD)

## valor de kappa entre as respostas para as perguntas
df_ad_kappa <- df_ad %>% 
  mutate(
    is_senior = as.double(P2_g == 'Sênior'), is_pleno = as.double(P2_g == 'Pleno'), is_junior = as.double(P2_g == 'Júnior')
  ) %>% 
  select(contains('P7'), contains('is_')) %>% 
  mutate(across(everything(), factor, levels = c(0, 1))) %>% 
  colpair_map(.f = kap_vec)

## visualizando o valor de kappa entre todas as respostas
df_ad_kappa %>% 
  data.frame %>% 
  `rownames<-`(value = .$term) %>% 
  select(-term) %>% 
  heatmaply()

## quais sao as respostas mais correlacionadas?
df_ad_kappa %>% 
  shave() %>% 
  stretch() %>% 
  drop_na() %>% 
  filter(abs(r) >= 0.25) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('x' = 'pergunta_id')) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('y' = 'pergunta_id')) %>% 
  mutate(
    texto.x = ifelse(test = is.na(texto.x), yes = x, no = texto.x),
    texto.y = ifelse(test = is.na(texto.y), yes = y, no = texto.y),
  ) %>% 
  select(texto.x, texto.y, r) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_colour = r), arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_color_distiller(palette = 'RdYlBu', direction = 1) +
  theme_void()

# COMBINANDO ATUACOES: CIENCIA DE DADOS ---------------------------------------------

## preparando os dados dos cientistas de dados por nivel
df_ds_combined <- dados %>%
  # removendo tudo o que for gestor e pegando tudo o que for resposta dada por
  # cientistas de dados
  filter(P2_d == 0, !is.na(P8_a) | !is.na(P8_b) | !is.na(P8_c) | !is.na(P8_d)) %>% 
  # # selecionando apenas as colunas relacionadas às perguntas feitas por cientistas de dados
  select(P0, P2_g, contains('P8_a_'), contains('P8_b_'), contains('P8_c_'), contains('P8_d_')) %>% 
  # juntando com features
  left_join(y = df_features, by = 'P0') %>% 
  # juntando com as outras informacoes
  left_join(y = dados %>% 
              # removendo tudo o que for dado de gestor e outras atuacoes
              filter(P2_d == 0, P4_a != 'Outra') %>% 
              # selecionando apenas as colunas com as informacoes de atuacao geral que usaremos
              select(P0, contains('P4_b_'), contains('P4_d_'), contains('P4_f_'), contains('P4_g_'), contains('P4_h_')) %>% 
              select(-P4_h_x),
            by = 'P0'
  ) %>% 
  # dropando os NAs
  drop_na()

## temperatura da matriz
temp_DS_combined <- nestedtemp(comm = select(df_ds_combined, contains('P8'), contains('P4')))
plot(temp_DS_combined, kind = 'incidence', names = c(FALSE, TRUE))

## quantificando a contribuição do turnover e aninhamento de habilidades para a similaridade entre cientista de dados
beta.multi(select(df_ds_combined, contains('P8'), contains('P4')), index.family = 'jaccard')

## particionando a similaridade entre os cientistas de dados
beta_part_DS_combined <- beta.pair(select(df_ds_combined, contains('P8'), contains('P4')), index.family = 'jaccard')

## olhando o resultado da PERMANOVA
adonis(pluck(beta_part_DS_combined, 3) ~ P2_g * role, data = df_ds_combined)

## ajustando um PERMDISP aos dados
beta_disper_DS_combined <- betadisper(pluck(beta_part_DS_combined, 3), group = interaction(df_ds_combined$role, df_ds_combined$P2_g), type = 'centroid')
beta_disper_DS_combined
permutest(beta_disper_DS_combined, pairwise = TRUE)
## visualizando o resultado da PERMDISP
plot(beta_disper_DS_combined)

## valor de kappa entre as respostas para as perguntas
df_ds_combined_kappa <- df_ds_combined %>% 
  mutate(
    is_senior = as.double(P2_g == 'Sênior'), is_pleno = as.double(P2_g == 'Pleno'), is_junior = as.double(P2_g == 'Júnior')
  ) %>% 
  select(contains('P8'), contains('P4'), contains('is_')) %>% 
  mutate(across(everything(), factor, levels = c(0, 1))) %>% 
  colpair_map(.f = kap_vec)

## visualizando o valor de kappa entre todas as respostas
df_ds_combined_kappa %>% 
  data.frame %>% 
  `rownames<-`(value = .$term) %>% 
  select(-term) %>% 
  heatmaply()

## quais sao as respostas mais correlacionadas?
df_ds_combined_kappa %>% 
  shave() %>% 
  stretch() %>% 
  drop_na() %>% filter(y == 'is_senior') %>% arrange(desc(r))
filter(abs(r) >= 0.25) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('x' = 'pergunta_id')) %>% 
  left_join(y = select(dicionario, pergunta_id, texto), by = c('y' = 'pergunta_id')) %>% 
  mutate(
    texto.x = ifelse(test = is.na(texto.x), yes = x, no = texto.x),
    texto.y = ifelse(test = is.na(texto.y), yes = y, no = texto.y),
  ) %>% 
  select(texto.x, texto.y, r) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_colour = r), arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  scale_edge_color_distiller(palette = 'RdYlBu', direction = 1) +
  theme_void()

# REGRAS DE ASSOCIACAO --------------------------------------------------------------

## extraindo as regras de associacao
df_regras <- dados %>% 
  # removendo tudo o que for dado de gestor e outras atuacoes
  filter(P2_d == 0, P4_a != 'Outra') %>% 
  # selecionando apenas as colunas com as informacoes de atuacao geral que usaremos
  select(P0, P1_e_b, P1_h, P1_i, P2_f, P2_g, P4_a, contains('P4_b_'), contains('P4_d_'), contains('P4_f_'), contains('P4_g_'), contains('P4_h_'),
         contains('P6_a_'), contains('P6_b_'), contains('P7_a_'), contains('P7_b_'), contains('P7_d_'), contains('P8_a_'), contains('P8_b_'), 
         contains('P8_c_'), contains('P8_d_')) %>% 
  # juntando as features derivadas
  left_join(y = df_features, by = 'P0') %>% 
  # dropando algumas colunas
  select(-P4_h_x, -P4_d_n, -target, -role_AD, -role_DS, -role_ED) %>% 
  # passando numerica para palavras
  mutate(across(where(is.double), ~ ifelse(test = .x == 1, yes = 'sim', no = 'nao'))) %>% 
  # aninhando a tabela pela atuacao
  nest(dados_brutos = -P4_a) %>% 
  # 
  mutate(
    # 
    transacoes = map(.x = dados_brutos,
                     .f = ~ pivot_longer(data = .x, 
                                         cols = P1_e_b:tempo_experiencia, names_to = 'feature', values_to = 'valor', values_drop_na = TRUE) %>% 
                       filter(valor != 'nao') %>% 
                       mutate(combinacao = paste0(feature, ': ', valor)) %>% 
                       group_by(P0) %>% 
                       summarise(combinacao = list(combinacao)) %>% 
                       mutate(combinacao = setNames(object = combinacao, nm = P0)) %>% 
                       pull(combinacao)
    ),
    #
    transacoes = map(.x = transacoes, .f = transactions),
    #
    regras = map(.x = transacoes, .f = apriori, 
                 # setando as configuracoes do algoritmo
                 parameter = list(minlen = 2, maxlen = 10, support = 0.3, 
                                  conf = 0.4, target = 'rules'), 
                 # desligando a verbosidade do algoritmo
                 control  = list(verbose = FALSE)
    ),
    # extraindo as regras como um dataframe
    regras_df       = map(.x = regras, .f = DATAFRAME,
                          setStart = '', setEnd = '', itemSep = ';')
  ) %>% 
  #
  select(P4_a, regras_df) %>% 
  # desaninhando a coluna de regras por atuacao
  unnest(regras_df) %>% 
  # tratando as regras
  mutate(
    # parseando as colunas de fator para caractere
    LHS        = as.character(LHS),
    RHS        = as.character(RHS),
    # definindo o tamanho de cada regra: cada ';' separa duas cartas dentro de uma
    # regra no LHS, e ainda temos a regra no RHS. Portanto, o tamanho de cada regra
    # são 2 acrescido da quantidade de separadores existentes no LHS
    tamanho    = str_count(string = LHS, pattern = ';') + 2,
    # criando identificador unico para cada deck
    combinacao = str_split(string = paste0(LHS, ';', RHS), pattern = ';'),
    combinacao = map_chr(.x = combinacao, .f = ~ paste0(sort(.x), collapse = ' + '))
  ) %>% 
  # dropando as regras com lift menor que 1
  filter(lift > 1)
df_regras

# SENIORIDADE DOS CIENTISTAS DE DADOS - TODAS AS RESPOSTAS --------------------------
# o que faz a senioridade de um cientista de dados? ---------------------------------

## contando instancias em cada classe
count(df_ds_combined, P2_g)

## preparando os dados que serao usados para a modelagem
df_model <- select(df_ds_combined, -c(idade:salario), -contains('role_'), -area_dados, -tempo_experiencia, -target) %>% 
  rowwise() %>% 
  mutate(
    respostas_geral = sum(c_across(contains('P4'))),
    respostas_especificas = sum(c_across(contains('P8')))
  ) %>% 
  ungroup

## fazendo o split dos dados
set.seed(33)
df_split <- initial_split(data = df_model, prop = 0.8, strata = P2_g)

## criando base com resample
set.seed(42)
df_boots <- bootstraps(data = training(x = df_split), times = 10, strata = P2_g)

## definindo os modelos
# regressao logistica multinomial
model_mult <- multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine(engine = 'glmnet') %>% 
  set_mode(mode = 'classification')

## criando a receita
preproc_basico <- recipe(P2_g ~ ., data = training(x = df_split)) %>% 
  update_role(P0, new_role = 'id variable') %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  # step_interact(terms = ~ starts_with('tamanho_empresa'):starts_with('role') + starts_with('instrucao'):starts_with('role') + 
  #                 starts_with('instrucao'):starts_with('tamanho_empresa')) %>% 
  step_zv(all_predictors())

# testando um modelo ----------------------------------------------------------------

## criando workflow para a regressão multinomial
wf <- workflow() %>% 
  add_recipe(recipe = preproc_basico) %>% 
  add_model(spec = model_mult)

## grid search para o modelo simples
doParallel::registerDoParallel()
set.seed(33)
wf_grid_search <- wf %>% 
  tune_race_anova(resamples = df_boots, grid = 60, metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc),
                  control = control_race(burn_in = 5, verbose = TRUE))

## pegando os melhores modelos no grid search
wf_grid_search %>% 
  show_best(metric = 'mn_log_loss')

## finalizando o workflow com o melhor modelo
wf <- wf %>% 
  finalize_workflow(parameters = select_best(x = wf_grid_search, metric = 'mn_log_loss'))

## pegando as metricas do modelo final na base de teste
trained_model_mult <- wf %>% 
  last_fit(split = df_split, metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc))

## pegando a regressao multinomial treinada
trained_model_mult %>% 
  collect_metrics()

## olhando a matriz de confusao
trained_model_mult %>% 
  extract_workflow() %>% 
  augment(new_data = testing(df_split)) %>% 
  conf_mat(truth = P2_g, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap') +
  labs(title = 'Regressão Multinomial')

# testando muitos modelos -----------------------------------------------------------

## setando outros modelos que testaremos
# arvore de decisao
model_dt <- decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% 
  set_engine(engine = 'rpart') %>% 
  set_mode(mode = 'classification')
# knn
model_knn <- nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_mode(mode = 'classification') %>% 
  set_engine(engine = 'kknn') 
## random forest
model_rf <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_mode(mode = 'classification') %>% 
  set_engine(engine = 'ranger', 
             regularization.factor = tune(), 
             regularization.usedepth = tune(), 
             max.depth = tune(), 
             sample.fraction = tune()
  )
## xgboost
model_xgb <- boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
                        loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode('classification') %>% 
  set_engine(engine = 'xgboost', 
             early_stopping_rounds = tune(), 
             lambda = tune(), 
             alpha = tune())

## setando parametros dos modelos
# random forest
params_rf <- model_rf %>% 
  parameters() %>% 
  update(
    mtry            = mtry(range = c(1L, 15L)),
    max.depth       = tree_depth(),
    sample.fraction = sample_prop(range = c(0.6, 1))
  )
# xgboost
params_xgb <- model_xgb %>% 
  parameters() %>% 
  update(
    early_stopping_rounds  = stop_iter(),
    
  )

## criando workflowset
wfset <- workflow_set(preproc = list(preproc = preproc_basico), 
                      models = list(rf = model_rf, xgb = model_xgb, dt = model_dt, mult = model_mult, knn = model_knn)) %>% 
  option_add(param_info = params_rf, id = "preproc_rf") %>% 
  option_add(param_info = params_xgb, id = "preproc_xgb")

## definindo o grid
grid_ctrl <- control_race(parallel_over = 'everything', save_workflow = TRUE, burn_in = 5)

## fazendo grid search entre todos os modelos
doParallel::registerDoParallel()
set.seed(33)
wfset_grid_search <- wfset %>% 
  workflow_map(fn = 'tune_race_anova', resamples = df_boots, grid = 60, 
               metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc), 
               verbose = TRUE, control = grid_ctrl, seed = 42)

## metrica alvo
metrica_alvo <- 'mn_log_loss'

## pegando os melhores modelos
wfset_grid_search %>% 
  rank_results() %>% 
  filter(.metric == metrica_alvo) %>% 
  arrange(mean)

## modelo selecionado
modelo_selecionado <- 'preproc_mult'

## pegando o melhor modelo
melhores_parametros <- wfset_grid_search %>% 
  extract_workflow_set_result(id = modelo_selecionado) %>% 
  select_best(metric = metrica_alvo)

## ajustando uma ultima vez na base de teste
trained_model_final <- wfset_grid_search %>% 
  extract_workflow(modelo_selecionado) %>% 
  finalize_workflow(parameters = melhores_parametros) %>% 
  last_fit(split = df_split, metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc)) 

## olhando as metricas do modelo
trained_model_final %>% 
  collect_metrics()

## olhando a matriz de confusao
trained_model_final %>% 
  extract_workflow() %>% 
  augment(new_data = testing(df_split)) %>% 
  conf_mat(truth = P2_g, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap') +
  labs(title = modelo_selecionado)

# olhando a explicabilidade do modelo -----------------------------------------------

## todas as variaveis
trained_model_final %>% 
  extract_workflow() %>% 
  tidy %>% 
  filter(term != '(Intercept)', estimate != 0) %>% 
  left_join(y = dicionario, by = c('term' = 'pergunta_id')) %>% 
  mutate(texto = ifelse(test = is.na(texto), yes = term, no = texto)) %>% 
  group_by(class) %>% 
  slice_max(order_by = abs(estimate), n = 10) %>% 
  ungroup %>% 
  mutate(termo = str_wrap(string = texto, width = 50),
         termo = reorder_within(x = termo, within = class, by = estimate)) %>% 
  ggplot(mapping = aes(x = estimate, y = termo, fill = class)) +
  facet_wrap(~ class, scales = 'free') +
  geom_col() +
  scale_y_reordered()

## atuacao especifica
trained_model_final %>% 
  extract_workflow() %>% 
  tidy %>% 
  filter(term != '(Intercept)', estimate != 0) %>% 
  left_join(y = dicionario, by = c('term' = 'pergunta_id')) %>% 
  mutate(texto = ifelse(test = is.na(texto), yes = term, no = texto)) %>% 
  filter(parte == 'P8') %>% 
  group_by(class) %>% 
  slice_max(order_by = abs(estimate), n = 10) %>% 
  ungroup %>% 
  mutate(termo = str_wrap(string = texto, width = 50),
         termo = reorder_within(x = termo, within = class, by = estimate)) %>% 
  ggplot(mapping = aes(x = estimate, y = termo, fill = class)) +
  facet_wrap(~ class, scales = 'free') +
  geom_col() +
  scale_y_reordered()

## atuacao geral
trained_model_final %>% 
  extract_workflow() %>% 
  tidy %>% 
  filter(term != '(Intercept)', estimate != 0) %>% 
  left_join(y = dicionario, by = c('term' = 'pergunta_id')) %>% 
  mutate(texto = ifelse(test = is.na(texto), yes = term, no = texto)) %>% 
  filter(parte == 'P4') %>% 
  group_by(class) %>% 
  slice_max(order_by = abs(estimate), n = 10) %>% 
  ungroup %>% 
  mutate(termo = str_wrap(string = texto, width = 50),
         termo = reorder_within(x = termo, within = class, by = estimate)) %>% 
  ggplot(mapping = aes(x = estimate, y = termo, fill = class)) +
  facet_wrap(~ class, scales = 'free') +
  geom_col() +
  scale_y_reordered()

## outras alavancas
trained_model_final %>% 
  extract_workflow() %>% 
  tidy %>% 
  filter(term != '(Intercept)', estimate != 0) %>% 
  left_join(y = dicionario, by = c('term' = 'pergunta_id')) %>% 
  mutate(texto = ifelse(test = is.na(texto), yes = term, no = texto)) %>% 
  filter(is.na(parte)) %>% 
  group_by(class) %>% 
  slice_max(order_by = abs(estimate), n = 10) %>% 
  ungroup %>% 
  mutate(termo = str_wrap(string = texto, width = 50),
         termo = reorder_within(x = termo, within = class, by = estimate)) %>% 
  ggplot(mapping = aes(x = estimate, y = termo, fill = class)) +
  facet_wrap(~ class, scales = 'free') +
  geom_col() +
  scale_y_reordered()

# ANALISE DO ERRO -------------------------------------------------------------------------------------------------

## colocando as previsões no dataframe original usado para a modelagem
df_erros <- trained_model_final %>% 
  extract_workflow() %>% 
  augment(new_data = df_model) %>% 
  mutate(
    tipo =
      case_when(
        P2_g == 'Sênior' & .pred_class %in% c('Pleno', 'Júnior') ~ 'underperforming',
        P2_g == 'Pleno' & .pred_class == 'Júnior' ~ 'underperforming',
        P2_g == 'Pleno' & .pred_class == 'Sênior' ~ 'overperforming',
        P2_g == 'Júnior' & .pred_class %in% c('Pleno', 'Sênior') ~ 'overperforming',
        TRUE ~ 'correct'
      )
  )

## contando quantas instancias de cada caso temos
count(df_erros, tipo)

## filtrando apenas os erros
df_erros_model <- filter(df_erros, P2_g != .pred_class) %>% 
  select(-contains('.pred_'), -P2_g)

## criando split destes dados
set.seed(33)
df_split_erros <- initial_split(data = df_erros_model, prop = 0.8, strata = tipo)

## criando bootstrap
set.seed(33)
df_boots_erro <- bootstraps(data = training(x = df_split_erros), times = 10, strata = tipo)

## instanciando os modelos
# regressão logistica
modelo_lr <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode(mode = 'classification') %>% 
  set_engine(engine = 'glmnet')
# mars
modelo_mars <- mars(num_terms = tune(), prod_degree = tune()) %>% 
  set_mode(mode = 'classification') %>% 
  set_engine(engine = 'earth')

## instanciando receita
preproc_erro <- recipe(tipo ~ ., data = training(x = df_split_erros)) %>% 
  update_role(P0, new_role = 'id variable') %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_zv(all_predictors())

## instanciando workflowset
wfset_erro <- workflow_set(preproc = list(preproc = preproc_erro), 
                           models = list(mars = modelo_mars, 
                                         dt = model_dt, 
                                         lr = modelo_lr,
                                         rf = model_rf,
                                         xgb = model_xgb)) %>% 
  option_add(param_info = params_rf, id = "preproc_rf") %>% 
  option_add(param_info = params_xgb, id = "preproc_xgb")

## fazendo grid search entre todos os modelos
doParallel::registerDoParallel()
set.seed(33)
wfset_grid_search_erro <- wfset_erro %>% 
  workflow_map(fn = 'tune_race_anova', resamples = df_boots_erro, grid = 60, 
               metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc, accuracy), 
               verbose = TRUE, control = grid_ctrl, seed = 42)

## definindo metrica
metrica_alvo = 'mn_log_loss'

## pegando os melhores modelos
wfset_grid_search_erro %>% 
  rank_results() %>% 
  filter(.metric == metrica_alvo) %>% 
  arrange(mean)

## definindo algoritmo alvo
algoritmo_alvo <- 'preproc_lr'

## pegando o melhor modelo
melhores_parametros <- wfset_grid_search_erro %>% 
  extract_workflow_set_result(id = algoritmo_alvo) %>% 
  select_best(metric = metrica_alvo)

## ajustando uma ultima vez na base de teste
trained_model_final_erros <- wfset_grid_search_erro %>% 
  extract_workflow(algoritmo_alvo) %>% 
  finalize_workflow(parameters = melhores_parametros) %>% 
  last_fit(split = df_split_erros, metrics = metric_set(mn_log_loss, bal_accuracy, roc_auc, accuracy)) 

## olhando as metricas do modelo
trained_model_final_erros %>% 
  collect_metrics()

## olhando a matriz de confusao
trained_model_final_erros %>% 
  extract_workflow() %>% 
  augment(new_data = testing(df_split_erros)) %>% 
  conf_mat(truth = tipo, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap') +
  labs(title = modelo_selecionado)

## explicabilidade do modelo: 1 é cargo Senior de atuação Júnior, e o 0 é o contrario (cargo Junior, atuação Sênior)
## estimates negativos são entao caracteristicos de quem tem cargo de Junior mas deveria ser Sênior
trained_model_final_erros %>% 
  extract_workflow() %>% 
  tidy() %>% 
  filter(estimate != 0) %>% 
  left_join(y = dicionario, by = c('term' = 'pergunta_id')) %>% 
  arrange(-abs(estimate)) %>% 
  select(term, estimate, texto) 

# ANÁLISE DE SIMILARIDADE DO ERRO ---------------------------------------------------------------------------------

## calculando dissimilaridade entre as respostas erradas
df_erros_diss <- df_erros_model %>% 
  relocate(role, tamanho_empresa, instrucao, .before = respostas_geral) %>% 
  mutate(across(P8_a_a:role, as.factor)) %>%
  mutate(
    tamanho_empresa       = factor(x = tamanho_empresa, levels = c('Microempresa', 'Pequeno porte', 'Médio porte', 'Grande porte')),
    tamanho_empresa       = as.ordered(tamanho_empresa),
    instrucao             = factor(x = instrucao , levels = c('Sem diploma', 'Graduação/Bacharelado', 'Pós-graduação', 'Mestrado', 'Doutorado ou Phd')),
    instrucao             = as.ordered(instrucao),
    respostas_geral       = (respostas_geral - mean(respostas_geral)) / sd(respostas_geral),
    respostas_especificas = (respostas_especificas - mean(respostas_especificas)) / sd(respostas_especificas)
  ) %>% 
  select(-P0, -tipo) %>% 
  daisy(metric = 'gower')

## clusterizando com um PAM
map_dbl(.x = 2:20,
        .f = ~ pam(x = df_erros_diss, k = .x, diss = TRUE)$silinfo$avg.width) %>% 
  plot(type = 'l')

## usando o Tsne para a visualização
Rtsne(X = df_erros_diss, is_distance = TRUE, perplexity = 10) %>% 
  pluck('Y') %>% 
  data.frame %>% 
  mutate(
    tipo          = df_erros_model$tipo,
    P2_g          = filter(df_erros, P2_g != .pred_class)$P2_g,
    .pred_class   = filter(df_erros, P2_g != .pred_class)$.pred_class,
    combinacao    = paste0('previu ', .pred_class, ' mas é ', P2_g)
  ) %>% 
  ggplot(mapping = aes(x = X1, y = X2, color = combinacao)) +
  geom_point(size = 3) +
  scale_color_viridis_d(begin = 0.1, end = 0.9)

# POR QUE ERROU? --------------------------------------------------------------------------------------------------
# coeficientes do sklearn -----------------------------------------------------------------------------------------

## olhando os coeficientes por classe
coeficients_sklearn %>% 
  # passando a base para o formato longo
  pivot_longer(cols = -index, names_to = 'coeficientes', values_to = 'valores') %>% 
  # removendo os coeficientes que foram zerados
  filter(valores != 0) %>% 
  # tratando a string do nome das coeficientes, para podermos juntar com o que veio do tidymodels
  mutate(
    coeficientes = str_remove(string = coeficientes, pattern = 'remainder__'),
    coeficientes = str_remove(string = coeficientes, pattern = 'onehotencoder__'),
    coeficientes = str_replace_all(string = coeficientes, pattern = '\\s', replacement = '.'),
    coeficientes = str_replace_all(string = coeficientes, pattern = '\\/', replacement = '.')
  ) %>% 
  left_join(y = dicionario, by = c('coeficientes' = 'pergunta_id')) %>% 
  mutate(texto = ifelse(test = is.na(texto), yes = coeficientes, no = texto)) %>% 
  group_by(index) %>% 
  slice_max(order_by = abs(valores), n = 10) %>% 
  ungroup %>% 
  mutate(termo = str_wrap(string = texto, width = 50),
         termo = reorder_within(x = termo, within = index, by = valores)) %>% 
  ggplot(mapping = aes(x = valores, y = termo, fill = index)) +
  facet_wrap(~ index, scales = 'free') +
  geom_col() +
  scale_y_reordered()

  ## os coeficientes são muito similares entre o sklearn e o tidymodels
coeficients_sklearn %>% 
  # passando a base para o formato longo
  pivot_longer(cols = -index, names_to = 'coeficientes', values_to = 'valores') %>% 
  # removendo os coeficientes que foram zerados
  filter(valores != 0) %>% 
  # tratando a string do nome das coeficientes, para podermos juntar com o que veio do tidymodels
  mutate(
    coeficientes = str_remove(string = coeficientes, pattern = 'remainder__'),
    coeficientes = str_remove(string = coeficientes, pattern = 'onehotencoder__'),
    coeficientes = str_replace_all(string = coeficientes, pattern = '\\s', replacement = '.'),
    coeficientes = str_replace_all(string = coeficientes, pattern = '\\/', replacement = '.')
  ) %>% 
  # juntando com os coeficientes ajustados pelo tidymodels, pegando so os coeficientes que tem em comum
  inner_join(y = trained_model_final %>% 
               extract_workflow() %>% 
               tidy %>% 
               filter(term != '(Intercept)', estimate != 0),
             by = c('index' = 'class' , 'coeficientes' = 'term')) %>%
  # aninhando o dataframe
  nest(data = everything()) %>% 
  # ajustando um teste t pareado aos dados
  mutate(teste_t = map(.x = data, .f = ~ t.test(x = .x$estimate, y = .x$valores, paired = TRUE))) %>% 
  # pegando os resultados do teste-t
  pull(teste_t)

# contra-factuais -------------------------------------------------------------------------------------------------

## quantidade de contra-factuais criados
contrafactuais %>% 
  # contando quantos contractuais existem por combinação subject-contractual criado
  count(Subject, CF, name = 'CFs') %>% 
  # contando quantos elementos existem em cada contrafactual criado
  count(Subject, CFs, name = 'observacoes') %>% 
  # agrupando pela quantidade de counterfactuals criadas
  group_by(CFs) %>% 
  # contando quantos registros existem pela quantidade de contrafactuais criados
  summarise(observacoes = sum(observacoes)) %>% 
  # plotando a distribuição da quantidade de contrafactuais necessarios para fazer com que a previsão acerte a classe alvo
  ggplot(mapping = aes(x = CFs, y = observacoes)) +
  geom_col()

## contrafactuais mais importantes por transição
contrafactuais %>%
  # contando quantas vezes cada o contra-factual de cada feature aparece por instancia
  count(Subject, Feature, Counter, name = 'ocorrencias') %>% 
  # pegando a proporção de vezes que o contra-factual de cada feature apareceu em cada instância
  mutate(proporcao = ocorrencias / 100) %>% 
  # juntando o target e o valor previsto originalmente para cada instancia
  left_join(y = distinct(contrafactuais, Subject, Target, Prediction), by = 'Subject') %>% 
  # agrupando pelo par target-previsao, feature e contra-factual
  group_by(Target, Prediction, Feature, Counter) %>% 
  # pegando a média da proporção de representativa do contra-factual
  summarise(media = mean(proporcao), .groups = 'drop') %>% 
  # agrupando pelo tipo de erro ocorrido (target-previsao)
  group_by(Target, Prediction) %>% 
  # pegando os dois contrafactuais mais frequentes em cada tipo de erro ocorrido
  slice_max(order_by = media, n = 2, with_ties = FALSE) %>% 
  # organizando o resultado em torno do contra-factual mais frequente por tipo de erro
  arrange(Target, Prediction, -media) %>% 
  # juntando o texto original de descrição da feature
  left_join(y = dicionario, by = c('Feature' = 'pergunta_id'))
