
# pergunta-alvo ---------------------------------------------------------------------

## o que diferencia uma pessoa de dados senior, pleno e junior?

# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse) # core
library(janitor) # para ajudar a limpar os dados
library(vegan) # para analise
library(betapart) # para decomposicao da diversidade beta
library(factoextra) # para clusterizacao
library(arules) # para regras de associacao

# carregando os dados ---------------------------------------------------------------

## carregando os dados as is
dados <- read_csv(file = '_posts/2022-12-31-state-of-data-2021/data/raw/State of Data 2021 - Dataset - Pgina1.csv')

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

## temperatura da matriz
temp_GEN <- nestedtemp(comm = select(df_general, P4_b_a:P4_h_v))
plot(temp_GEN, kind = 'incidence', names = c(FALSE, TRUE))

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

## temperatura da matriz
temp_DS <- nestedtemp(comm = select(df_ds, contains('P8')))
plot(temp_DS, kind = 'incidence', names = c(FALSE, TRUE))

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

## temperatura da matriz
temp_EN <- nestedtemp(comm = select(df_en, contains('P6')))
plot(temp_EN, kind = 'incidence', names = c(FALSE, TRUE))

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

## temperatura da matriz
temp_AD <- nestedtemp(comm = select(df_ad, contains('P7')))
plot(temp_AD, kind = 'incidence', names = c(FALSE, TRUE))

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

## temperatura da matriz
temp_DS_combined <- nestedtemp(comm = select(df_ds_combined, contains('P8'), contains('P4')))
plot(temp_DS_combined, kind = 'incidence', names = c(FALSE, TRUE))

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

# CLUSTERIZAÇÃO ---------------------------------------------------------------------

dados %>% 
  # removendo tudo o que for dado de gestor e outras atuacoes
  filter(P2_d == 0, P4_a != 'Outra') %>% 
  # selecionando apenas as colunas com as informacoes de atuacao geral que usaremos
  select(P0, P1_h:P2_c, P2_h, P2_j, P3_a, P4_a, contains('P4_b_'), contains('P4_d_'), contains('P4_f_'), contains('P4_g_'), contains('P4_h_')) %>% 
  select(-P4_h_x, -P4_d_n)
