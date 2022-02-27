
# limpando o ambiente antes de comecar ----------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

# carregando os pacotes
library(tidyverse) # core
library(tidytext) # para manipular texto
library(stringi) # para trabalhar com texto
library(stm) # para o STM
library(furrr) # para paralelizar o STM
library(plotly) # para graficos interativos
library(Rtsne) # para calcular o TSNE
library(widyr) # para ajudar a calcular o nearest neighbors
library(reactable) # para tabelas interativas
library(reactablefmtr) # para embedar imagens nas tabelas

# definindo funcoes -----------------------------------------------------------------

# funcao para calcular o nearest neighbors para um determinado input
nearest_neighbors <- function(df, carta, vizinhos) {
  
  # calculando a similaridade de coseno entre todas as cartas e a carta alvo
  df %>%
    # aplicando a funcao
    widely(
      ~ {
        # cria matriz n x m, onde n eh o numero de cartas que existem na base de dados, e m
        # é o número de tópicos identificados através do STM - o conteúdo de cada célular na
        # matriz é a probabilidade de que àquela carta esteja associada aquele tópico
        y <- .[rep(carta, nrow(.)), ]
        # no codigo abaixo o '.' representa a matriz de probablidades de cada carta possuir
        # cada tópico, e é uma matriz n x m onde o n é cada uma das cartas e o m corresponde
        # a várias colunas que representam cada um dos tópicos. Calcularemos então a similaridade
        # do conseno a carta selecionado e o embedding representado por cada outra carta:
        # - rowSums(. * y): multiplica a matriz do embedding de todos as cartas pela matriz
        # da carta selecionada
        # - sqrt(rowSums(. ^ 2)): retorna um vetor numerico, com um elemento por carta o valor
        # associado à cada carta representa o somatorio dos valores entre todas as dimensoes
        # de seu embedding (i.e., todos os topicos associado àquela carta)
        # sqrt(sum(.[token, ] ^ 2)): retorna um valor numérico, que representa o somatório dos
        # valores entre todas as dimensoes do embedding para a carta selecionada
        # (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2))): multiplica o valor do embedding
        # de cada carta pelo da carta selecionado, padronizando a similaridade calculada
        # pelo 'rowSums(. * y)'
        similaridade_coseno <- rowSums(. * y) / (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[carta, ] ^ 2)))
        # coloca o resultado em uma matriz com o nome de linha vinda do nome das cartas
        #matrix(similaridade_coseno, ncol = 1, dimnames = list(x = names(similaridade_coseno)))
      },
      sort = TRUE
    )(document, topico, probabilidade) %>%
    # organizando as cartas em ordem decrescente de similaridade
    arrange(desc(item2)) %>% 
    # pegando apenas a quantidade desejada de cartas similares
    slice_max(order_by = item2, n = vizinhos) %>% 
    # juntando com metadados das cartas resultantes
    left_join(y = select(cartas, localizedName, slug, small, texto), by = c('item1' = 'localizedName'))
}

# carregando os dados ---------------------------------------------------------------

# carregando os dados
cartas <- read_rds(file = '_posts/2022-01-31-card-embeddings-parte-1/data/cartas.rds')

# definindo a paleta de cores por faccao
cores_por_faccao <- c('Monsters' = 'red', 'Nilfgaard' = 'grey30', 'Northern Realms' = 'deepskyblue2', 
                      "Scoia'tael" = 'forestgreen', 'Skellige' = 'purple3', 'Syndicate' = 'orange2',
                      'Neutral' = 'tan4')

# ajustando os dados antes de prosseguir --------------------------------------------

# ajustando a tabela por conta de duas cartas má registradas
cartas <- cartas %>% 
  # removendo a carta Solução Engenhosa, que aparece duas vezes por conta de diferencas
  # em seu nome em ingles
  filter(!(localizedName == 'Solução engenhosa' & name != 'Blueprint')) %>% 
  # ajustando o nome da carta Vidente, que aparece duas vezes pois existe uma na facção
  # neutra e outra na Scoia'tael, mas sao cartas diferentes
  mutate(
    localizedName = case_when(localizedName == 'Vidente' ~ paste0(localizedName, ' (', slug, ')'),
                              TRUE ~ localizedName)
  ) %>% 
  # colocando as cartas em ordem alfabetica
  arrange(localizedName)
cartas

# filtrando apenas a faccao alvo ----------------------------------------------------

# definindo o string da faccao que vamos utilizar
faccao_alvo <- "Scoia'tael"

# filtrando apenas as cartas desta faccao-alvo e as cartas neutras (que podem ser usadas juntas)
cartas <- filter(cartas, slug %in% c(faccao_alvo, 'Neutral'))

# pre-processando os dados ----------------------------------------------------------

# lista de palavras para remover
my_stopwords <- c('a', 'ao', 'aos', 'ate', 'cada', 'com', 'as', 'como', 'da', 'das', 
                  'de', 'dela', 'delas', 'dele', 'desta',  'deste', 'destas', 'destes',
                  'deles', 'do', 'dos', 'disso', 'e', 'es', 'em', 'esta',  'ela', 'ele',
                  'elas', 'eles', 'for', 'foi', 'la', 'lhe', 'mais', 'nas', 'nesta', 
                  'na', 'nas', 'nela', 'nele', 'no', 'nos', 'o', 'os', 'ou', 'para',
                  'por', 'pelo', 'que', 'sao', 'se', 'so', 'sos', 'sem', 'seu', 'seus',
                  'sua', 'suas', 's', 'si', 'todas', 'todos', 'tem', 'um', 'uma', 'voce',
                  'vez', 'longa', 'distancia', 'corpo', 'duas', 'dois', 'metade', 'reinos',
                  'norte', "scoia'tael", 'skellige', 'nilfgaard', 'sindicato', 'neutra',
                  'concede', 'tiver', 'seguida', 'seja', 'caso', 'faz', 'usa', 'usar',
                  'usando', 'usada', 'usado', 'tambem', 'houver', 'ha', 'pela', 'mesma',
                  'tiver', 'nao', 'nessa', 'nessas', 'nesse', 'nesses', 'qualquer', 
                  'estiver', 'entre', 'unidade', 'unidades', 'mobilizacao', 'sempre', 
                  'mesmo', 'perto', 'apos', 'quando', 'neste', 'nestes', "scoia'tel",
                  'enquanto')

# regex da frase que precisaremos remover
txt <- paste('Esta habilidade adiciona [0-9]{2} (?:(?:de )?recrutamento[s]?',
             'ao limite )?de recrutamento (ao limite )?do (?:seu )?baralho.')

# contando ocorrencias de cada token por faccao
df_tokens <- cartas %>% 
  # removendo texto comum a todas as cartas de habilidade do lider
  mutate(
    texto = str_remove(string = texto, pattern = txt)
  ) %>% 
  # quebrando o string em tokens
  unnest_tokens(output = token, input = texto) %>% 
  # removendo acentuacao
  mutate(token = stri_trans_general(str = token, id = 'Latin-ASCII')) %>%
  # removendo stopwords e os digitos
  filter(!token %in% my_stopwords,
         str_detect(string = token, pattern = '[0-9]', negate = TRUE)) %>% 
  # substituindo algumas as formas de algumas palavras
  mutate(
    # removendo o plural de algumas palavras em especifico
    token = str_replace(string = token, pattern = '(?<=o|a)s$', replacement = ''),
    token = str_replace(string = token, pattern = '(?<=d|t)es$', replacement = 'e'),
    token = str_replace(string = token, pattern = '(?<=r)es$', replacement = ''),
    # padronizando a escrita de algumas habilidades e condicoes
    token = str_replace(string = token, pattern = 'veneno|envenenamento|envenenad[ao]', replacement = 'envenena'),
    token = str_replace(string = token, pattern = 'bloqueada|bloquei[ao]', replacement = 'bloqueio'),
    token = str_replace(string = token, pattern = 'reforcad[ao]', replacement = 'reforcada'),
    # padronizando a escrita de outras palavras
    token = str_replace(string = token, pattern = 'anoes', replacement = 'anao'),
    token = str_replace(string = token, pattern = 'aleatoria(?:mente)?', replacement = 'aleatorio')
  ) %>% 
  # contando ocorrencia dos lemmas por carta
  count(localizedName, token, name = 'ocorrencias') 
df_tokens

# criando matriz esparsa
df_esparsa <- cast_sparse(data = df_tokens, row = localizedName, column = token, value = ocorrencias)

# criando matriz de features --------------------------------------------------------

# listando todas as habilidades associadas a um status
hab_status <- c('bleeding', 'blood_moon', 'bounty', 'defender', 'doomed', 'immune', 
                'lock', 'poison', 'resilient', 'rupture', 'shield', 'spying', 'vitality',
                'veil')

# listando todas as habilidade que causam algum tipo de efeito de area
hab_aoe <- c('blood_moon', 'cataclysm', 'dragons_dream', 'fog', 'frost', 'rain', 'storm')

# criando tabela com as covariaveis de cada carta
df_covariaveis <- cartas %>% 
  # codificando os dois grupos bem marcados de habilidades e criando um nivel para tudo o que
  # nao se encaixa naqueles dois
  mutate(
    # habilidades associadas que causam ou dao um status a carta
    habilidade_status = str_detect(string = keywords, 
                                   pattern = paste0(hab_status, collapse = '|')),
    # habilidades com efeito de area
    habilidade_aoe = str_detect(string = keywords, 
                                pattern = paste0(hab_aoe, collapse = '|')),
    # todas as habilidades que não se encaixarem nas duas ultimas
    habilidade_outras = str_detect(string = keywords, negate = TRUE,
                                   pattern = paste0(c(hab_status, hab_aoe), collapse = '|'))
  ) %>% 
  # selecionando apenas as covariaveis que vamos usar
  select(localizedName, slug, contains('habilidade')) %>% 
  # substituindo os valores faltantes nas colunas das habilidades por FALSE
  mutate(across(.cols = contains('habilidade'), .fns = replace_na, FALSE))
df_covariaveis

# buscando e extraindo o melhor modelo ----------------------------------------------

# setando a seed
set.seed(33)

# setando o processamento paralelo
plan(multisession)

# buscando melhor valor de K
search_K <- tibble(
  K = seq(from = 6, to = 30, by = 3)
) %>% 
  mutate(
    # rodando o STM padrao
    Nenhuma = future_map(.x = K, 
                         .f = ~ stm(documents = df_esparsa, init.type = 'Spectral', 
                                    seed = 333, K = .x, verbose = FALSE),
                         .options = furrr_options(seed = TRUE)
    ),
    # rodando o STM com covariaveis apenas para o conteudo dos topicos
    Conteudo = future_map(.x = K, 
                          .f = ~ stm(documents = df_esparsa, init.type = 'Spectral', 
                                     seed = 333, K = .x, content = ~ slug, data = df_covariaveis,
                                     verbose = FALSE),
                          .options = furrr_options(seed = TRUE)
    ),
    # rodando o STM com covariaveis apenas para a prevalencia dos topicos
    Prevalencia = future_map(.x = K, 
                             .f = ~ stm(documents = df_esparsa, init.type = 'Spectral', 
                                        seed = 333, K = .x, 
                                        prevalence = ~ habilidade_status + habilidade_aoe + habilidade_outras, 
                                        data = df_covariaveis, verbose = FALSE),
                             .options = furrr_options(seed = TRUE)
    ),
    # rodando o STM com covariaveis para o conteudo o prevalencia dos topicos
    Ambas = future_map(.x = K, 
                       .f = ~ stm(documents = df_esparsa, init.type = 'Spectral', 
                                  seed = 333, K = .x, content = ~ slug,
                                  prevalence = ~ habilidade_status + habilidade_aoe + habilidade_outras, 
                                  data = df_covariaveis, verbose = FALSE),
                       .options = furrr_options(seed = TRUE)
    )
  ) %>% 
  pivot_longer(cols = c(Nenhuma, Conteudo, Prevalencia, Ambas), names_to = 'tipo', values_to = 'modelos')

# setando o processamento sequencial
plan(sequential)

# visualizando os resultados da busca para selecionar o melhor modelo
# extraindo as metricas de avaliacao da clusterizacao
search_K %>% 
  # calculando a exclusividade e a coerencia dos topicos
  mutate(
    coerencia     = map(.x = modelos, .f = semanticCoherence, documents = df_esparsa),
    exclusividade = map(.x = modelos, .f = safely(exclusivity)),
    exclusividade = map(.x = exclusividade, .f = 'result'),
    residuos      = map(.x = modelos, .f = checkResiduals, df_esparsa),
    residuos      = map(.x = residuos, 'dispersion')
  ) %>% 
  # dropando a coluna com os modelos
  select(-modelos) %>% 
  # desaninhando as colunas de coerencia e exclusividade
  unnest(cols = c(exclusividade, coerencia, residuos)) %>% 
  # passando a base para o formato longo
  pivot_longer(cols = c(exclusividade, coerencia, residuos), 
               names_to = 'metrica', values_to = 'valor') %>% 
  # dropando valores nulos
  drop_na() %>% 
  # agrupando pelo valor de K e da metrica
  group_by(K, metrica, tipo) %>% 
  # calculando o valor da media da metrica por valor de K
  summarise(
    valor = mean(x = valor, na.rm = TRUE), .groups = 'drop'
  ) %>% 
  # renomeando as metricas
  mutate(
    metrica = case_when(metrica == 'coerencia' ~ 'Coerência Semântica',
                        TRUE ~ str_to_title(string = metrica))
  ) %>% 
  # criando a figura
  ggplot(mapping = aes(x = as.factor(K), y = valor, group = tipo, color = tipo)) +
  facet_wrap(~ metrica, scales = 'free') +
  geom_line(size = 1, show.legend = TRUE) +
  geom_point(fill = 'white', color = 'black', shape = 21, size = 3, show.legend = FALSE) +
  labs(
    title    = 'Quantos tópicos devemos usar?', 
    subtitle = 'A quantidade de tópicos escolhida deve atender ao melhor balanço entre uma alta coerência semântica e exclusividade, mas baixos resíduos',
    x        = 'Quantidade de tópicos (K)',
    y        = 'Valor da métrica',
    color    = 'Covariáveis'
  ) +
  theme(
    legend.position = 'bottom'
  )

# extraindo o melhor modelo
modelo <- search_K %>% 
  # pegando o modelo selecionado
  filter(K == 15, tipo == 'Prevalencia') %>% 
  # extraindo o modelo selecionado
  pull(modelos) %>% 
  # tirando o modelo da lista
  pluck(1)

# extraindo os embeddings -----------------------------------------------------------

# pegando a matriz gamma - as probabilidade de cada topico por documento
embeddings <- tidy(x = modelo, matrix = 'gamma') %>% 
  # juntando o prefixo topic_ ao numero de cada topico
  mutate(topic = paste0('topic_', topic)) %>% 
  # pivoteando a tabela para o formato largo
  pivot_wider(id_cols = document, names_from = topic, values_from = gamma) %>% 
  # agrupando o dataframe por linha
  rowwise() %>% 
  # extraindo o topico mais provavel por linha
  mutate(
    topK = which.max(c_across(contains('topic_'))),
    topK = ifelse(test = topK < 10, yes = paste0('Tópico 0', topK), no = paste0('Tópico ', topK))
  ) %>% 
  # desagrupando o dataframe
  ungroup %>% 
  # colocando o nome das cartas na coluna do nome do documento
  mutate(document = cartas$localizedName) %>% 
  # juntando os metadados das cartas
  left_join(y = cartas, by = c('document' = 'localizedName'))
embeddings

# visualizando e testando os embeddings ---------------------------------------------

# setando a seed
set.seed(33)

# ajustando o TSNE
tsne_results <- select(embeddings, contains('topic_')) %>% 
  # passando objeto para matrix
  as.matrix() %>% 
  # ajustando tSNE
  Rtsne(check_duplicates = FALSE, perplexity = 20)

# plotando resultados do TSNE
tsne_results %>% 
  # pegando os resultado do TSNE
  pluck('Y') %>% 
  # passando para um dataframe
  data.frame %>% 
  # renomeando as colunas
  `names<-`(value = c('tsne1', 'tsne2')) %>% 
  # passando para um tibble
  tibble %>% 
  # juntando com o nome das cartas
  bind_cols(embeddings) %>% 
  # criando a figura
  plot_ly(x = ~ tsne1, y = ~ tsne2, color = ~ slug, data = ., colors = cores_por_faccao,
          mode = 'markers', type = 'scatter', marker = list(size = 7, opacity = 0.7),
          hoverinfo = 'text', 
          hovertext = ~ paste0(
            '<b>Tópico prevalente:</b> ', topK, '<br>',
            '<b>Carta:</b> ', document, '<br>',
            '<b>Raridade:</b> ', rarity, '<br>',
            '<b>Tipo:</b> ', type, '<br>',
            str_wrap(string = texto, width = 50)
          )
  ) %>% 
  layout(xaxis = list(title = 'Dimensão 1'), yaxis = list(title = 'Dimensão 2'))

# colocando os embeddings no formato para a funcao abaixo
df_embedding <- select(embeddings, document, contains('topic_')) %>% 
  # passando a base para o formato longo
  pivot_longer(cols = contains('topic_'), names_to = 'topico', values_to = 'probabilidade')

# testando o embedding
df_embedding %>% 
  # calculando o nearest neighbors
  nearest_neighbors(carta = 'Bruxo Gato', vizinhos = 5) %>% 
  # selecionando as colunas que vamos plotar
  select(small, item1, item2, texto) %>% 
  # adicionando o prefixo do link para a imagem
  mutate(small = paste0('https://www.playgwent.com/', small)) %>%
  # colocando os exemplos em um reactable
  reactable(
    compact = TRUE, borderless = TRUE, defaultColDef = colDef(align = 'left'), 
    style = list(fontFamily = "Roboto", fontSize = "12px"),
    columns = list(
      small = colDef(name = '', cell = embed_img(height = 80, width = 60), maxWidth = 80),
      item1 = colDef(name = 'Carta', maxWidth = 90),
      item2 = colDef(name = 'Similaridade', maxWidth = 90, format = colFormat(digits = 3)),
      texto = colDef(name = 'Descrição')
    )
  )

# salvando os outputs ---------------------------------------------------------------

## escrevendo o objeto do modelo
write_rds(x = modelo, file = '_posts/2022-01-31-card-embeddings-parte-1/modelos/stm_scoiatael.rds')
## escrevendo os embeddings
write_rds(x = df_embedding, file = '_posts/2022-01-31-card-embeddings-parte-1/data/embeddings_scoiatael.rds')
