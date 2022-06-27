
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

library(tidyverse)
library(httr)
library(xml2)
library(fs)

# carregando dados ------------------------------------------------------------------

## lendo os dados do ranking da BGG para pegar o identificador unico de cada jogo
ranking <- read_rds(file = '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/ranking_bgg.rds')

# criando e setando paths -----------------------------------------------------------

## setando o path onde vamos jogar os arquivos
path_scrapped_data <- '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/temporario_bgg_metadata/'
path_scrapped_expansions <- '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/temporario_bgg_expansoes/'

## criando pasta se ela nao existir
if(!dir_exists(path_scrapped_data)){
  dir_create(path = path_scrapped_data, recurse = TRUE)
}

## criando pasta se ela nao existir
if(!dir_exists(path_scrapped_expansions)){
  dir_create(path = path_scrapped_expansions, recurse = TRUE)
}

# definindo funcoes -----------------------------------------------------------------

## funcao para pegar o XML de um jogo
pega_jogo <- function(game_id, path_to_save = NULL) {
  # url base para pegar um jogo, as estatisticas e a primeira pagina de comentarios
  base_url <- str_glue('https://www.boardgamegeek.com/xmlapi2/thing?id={game_id}&stats=1&ratingcomments=1&pagesize=100')
  # path para salvar o arquivo
  file_path = sprintf(fmt = '%s/%08d.html', path_to_save, game_id)
  Sys.sleep(time = runif(n = 1, min = 0.8, max = 1.2))
  # fazendo o request e salvando o codigo da resposta se o path nao for nulo
  requisicao <- GET(url = base_url, write_disk(path = file_path, overwrite = TRUE))
  if(requisicao$status_code != 200 | map_lgl(.x =  xml_text(content(requisicao)), .f = str_detect, pattern = 'Rate limit exceeded')) {
    usethis::ui_stop('Não foi possível baixar o arquivo: status code diferente de 200 ou limite de requisições atingido.')
  }
}

## funcao para parsear a lista de nomes do jogo
parser_nome <- function(arquivo_xml) {
  # pega o arquivo HTML
  arquivo_xml %>% 
    # extrai todas as tags name
    xml_find_all(xpath = '*//name') %>% 
    # extrai todo os atributos dessas tags
    xml_attrs('value') %>% 
    # junta todos os atributos em uma tibble
    bind_rows() %>% 
    # remove a coluna sortindex
    select(titulo = value, metadado = type)
}

## funcao para parsear a descricao do jogo
parser_descricao <- function(arquivo_xml) {
  # pega o arquivo HTML
  arquivo_xml %>% 
    # extrai todas as tags description
    xml_find_first(xpath = '*//description') %>% 
    # extrai o texto da descricao
    xml_text()
}

## funcao para parsear os metadados do jogo
parser_metadados <- function(arquivo_xml) {
  # pega o arquivo HTML
  metadata_extraction <- arquivo_xml %>% 
    # extrai todas as tags link
    xml_find_all(xpath = '*//link') %>% 
    # extrai todo os atributos dessas tags
    xml_attrs()
  
  # tratando os metadados apenas se eles existirem 
  if(length(metadata_extraction) == 0){
    NULL
  } else {
    metadata_extraction %>% 
      # junta todos os atributos em uma tibble
      bind_rows() %>% 
      # organiza as colunas
      select(id_metadado = id, metadado = value, tipo_metadado = type) %>% 
      # removendo o padrao boardgame do tipo de metadado
      mutate(tipo_metadado = str_replace(string = tipo_metadado, pattern = 'boardgame', replacement = 'tbl_')) %>% 
      # aninhando informacoes pelo tipo de metadado
      nest(data = -tipo_metadado) %>% 
      # passando o dado para o formato largo
      pivot_wider(names_from = tipo_metadado, values_from = data)
  }
}

## funcao para parsear as informacoes do jogo
parser_informacoes <- function(arquivo_xml) {
  # pega o arquivo HTML
  arquivo_xml %>% 
    # extrai todas as tags relacionadas às informações sobre o ano de publicacao, quantidade de jogadores
    # idade minima para o jogo e tempo de jogo
    xml_find_all(xpath = '*//*[self::yearpublished or self::minplayers or self::maxplayers
               or self::playingtime or self::minplaytime or self::maxplaytime or self::minage]') %>% 
    # colocando todos os atributos dessa tag em um tibble
    map_dfr(
      ~ list(
        caracteristica = xml_name(.x),
        valor = xml_attrs(.x, 'value')
      )) %>% 
    # parseando tudo para numerico
    mutate(valor = parse_number(x = valor)) %>% 
    # passando o tibble do formato longo para o largo
    pivot_wider(names_from = caracteristica, values_from = valor)
}

## funcao para parsear todas as informacoes relacionadas à avaliação de cada jogo
parser_avaliacoes <- function(arquivo_xml) {
  # extrai outras informacoes das avaliacoes e junta com as informacoes de rankings e contagem de comentarios
  arquivo_xml %>% 
    # extrai todas as tags relacionadas dentro das avaliacoes que nao estejam relacionadas ao rankeamento
    xml_find_all(xpath = '*/statistics/ratings/*[not(self::ranks)]') %>% 
    # coloca todas as informacoes dentro de um tibble
    map_dfr(
      ~ list(
        estatistica = xml_name(.x),
        valor = xml_attrs(.x, 'value')
      )
    ) %>% 
    # parseando tudo para numerico
    mutate(valor = parse_number(x = valor)) %>% 
    # passando o tibble do formato longo para o largo
    pivot_wider(names_from = estatistica, values_from = valor)
}

## funcao para parsear todas as informacoes relacionadas aos rankings em que cada jogo esta
parser_rankings <- function(arquivo_xml) {
  # pega o arquivo html
  arquivo_xml %>% 
    # extrai todas as tags que estejam relacionadas ao ranking
    xml_find_all(xpath = '*/statistics/ratings/ranks/rank') %>% 
    # extrai todo os atributos dessas tags
    xml_attrs('value') %>% 
    # junta todos os atributos em uma tibble
    bind_rows() %>% 
    # renomeando colunas
    rename(nivel = type, tipo = name, nome = friendlyname, 
           posicao = value, media_bayesiana = bayesaverage) %>% 
    # parseando os numericos para tal
    mutate(
      posicao         = parse_number(x = posicao, na = c('', 'NA', 'Not Ranked')),
      media_bayesiana = parse_number(x = media_bayesiana, na = c('', 'NA', 'Not Ranked'))
    )
}

## funcao para parsear os comentarios sobre o jogo
parser_comentarios <- function(arquivo_xml) {
  # pega o arquivo HTML
  arquivo_xml %>% 
    # extrai todas as tags de comentario
    xml_find_all(xpath = '*/comments/comment') %>% 
    # extrai todo os atributos dessas tags
    xml_attrs('value') %>% 
    # junta todos os atributos em uma tibble
    bind_rows() %>% 
    # renomeando as colunas
    rename(usuario = username, nota = rating, comentario = value) %>% 
    # parseando as notas para numerico
    mutate(nota = parse_number(x = nota))
}

## funcao para parsear a quantidade total de comentarios que um jogo tem
parser_comentarios_total <- function(arquivo_xml) {
  # pegando o total de comentarios
  n_comentarios <- arquivo_xml %>% 
    # pega tudo o que está sobre a tag comments
    xml_find_all(xpath = '*/comments') %>% 
    # pega apenas o valor correspondente ao total de comentarios
    xml_attr('totalitems') %>% 
    # parseando o string para numero
    parse_number()
  # colocando os comentarios em uma tibble
  tibble(total_comentarios = ifelse(test = length(n_comentarios) == 0, yes = 0, no = n_comentarios))
}

## funcao para parsear os resultados da votacao do melhor numero de jogadores para se jogar
parser_votacao_n_jogadores <- function(arquivo_xml) {
  # pega o arquivo html
  arquivo_xml %>% 
    # extrai todas as tags com os resultados da votação relacionada ao melhor numero de jogadores
    xml_find_all(xpath = '*/poll[@name="suggested_numplayers"]/results') %>% 
    # coloca tudo dentro de um tibble
    map_dfr(
      ~ xml_find_all(.x, xpath = 'result') %>% 
        xml_attrs() %>% 
        bind_rows() %>% 
        mutate(numplayers = xml_attrs(.x))
    )  %>% 
    # renomeia e organiza as colunas
    select(num_jogadores = numplayers, voto = value, num_votos = numvotes) %>% 
    # parseando votos para numerico
    mutate(num_votos = parse_number(x = num_votos))
}

## funcao para parsear os resultados da votacao da idade recomendada para o jogo
parser_votacao_idade <- function(arquivo_xml) {
  # pega o arquivo html
  arquivo_xml %>% 
    # extrai todas as tags com os resultados da votação relacionada à idade recomendada para o jogo
    xml_find_all(xpath = '*/poll[@name="suggested_playerage"]/results') %>% 
    # coloca tudo dentro de um tibble
    map_dfr(
      ~ xml_find_all(.x, xpath = 'result') %>% 
        xml_attrs()
    ) %>% 
    # renomeia e organiza as colunas
    select(idade_ideal = value, num_votos = numvotes) %>% 
    # parseando votos para numerico
    mutate(num_votos = parse_number(x = num_votos))
}

## funcao para parsear os resultados da votacao sobre a dependencia do idioma para jogar o jogo
parser_votacao_idioma <- function(arquivo_xml) {
  # pega o arquivo html
  arquivo_xml %>% 
    # extrai todas as tags com os resultados da votacao sobre a dependencia do idioma para jogar o jogo
    xml_find_all(xpath = '*/poll[@name="language_dependence"]/results') %>% 
    # coloca tudo dentro de um tibble
    map_dfr(
      ~ xml_find_all(.x, xpath = 'result') %>% 
        xml_attrs()
    ) %>% 
    # renomeia e organiza as colunas
    select(voto = value, num_votos = numvotes) %>% 
    # parseando votos para numerico
    mutate(num_votos = parse_number(x = num_votos))
}

## adicionando um possibly a algumas funcoes
possivel_votacao_idade <- possibly(.f = parser_votacao_idade, otherwise = tibble())
possivel_votacao_idioma <- possibly(.f = parser_votacao_idioma, otherwise = tibble())
possivel_votacao_jogadores <- possibly(.f = parser_votacao_n_jogadores, otherwise = tibble())
possivel_comentarios <- possibly(.f = parser_comentarios, otherwise = tibble())

# funcao para parsear o arquivo xml inteiro do jogo
parser_do_jogo <- function(path_arquivo_xml) {
  
  ## lendo o arquivo xml
  xml_do_jogo <- read_xml(x = path_arquivo_xml)
  
  ## parseando o arquivo xml
  tibble(
    tbl_nomes             = list(parser_nome(arquivo_xml = xml_do_jogo)),
    tbl_comentarios       = list(possivel_comentarios(arquivo_xml = xml_do_jogo)), 
    tbl_rankings          = list(parser_rankings(arquivo_xml = xml_do_jogo)), 
    tbl_votacao_idade     = list(possivel_votacao_idade(arquivo_xml = xml_do_jogo)), 
    tbl_votacao_idioma    = list(possivel_votacao_idioma(arquivo_xml = xml_do_jogo)),
    tbl_votacao_jogadores = list(possivel_votacao_jogadores(arquivo_xml = xml_do_jogo))
  ) %>% 
    # juntando informacoes que ja estao no formato esperado
    bind_cols(
      parser_metadados(arquivo_xml = xml_do_jogo),
      descricao = parser_descricao(arquivo_xml = xml_do_jogo),
      total_comentarios = parser_comentarios_total(arquivo_xml = xml_do_jogo),
      parser_avaliacoes(arquivo_xml = xml_do_jogo),
      parser_informacoes(arquivo_xml = xml_do_jogo)
    )
}

# pegando alguns jogos --------------------------------------------------------------

## id dos jogos que vamos pegar
ids_para_pegar <- ranking %>%  
  # pegando apenas os valores distintos
  distinct(id) %>% 
  # dropando NAs no id
  drop_na() %>% 
  # pegando o id deles
  pull(id) %>% 
  # parseando para numerico
  parse_number()
  
## funcao para pegar os metadados do jogo de forma segura
pega_jogo_insistente <- insistently(f = pega_jogo, rate = rate_delay(pause = 10, max_times = 5))

## pegando estes arquivos
walk(.x = ids_para_pegar,
     .f = pega_jogo_insistente, path_to_save = path_scrapped_data)

# parseando os jogos raspados -------------------------------------------------------

## listando todos os arquivos que pegamos
arquivos_raspados <- dir_ls(path = path_scrapped_data)

## paralelizando parsing
library(furrr)
plan(multisession)
## parseando os arquivos raspados
jogos <- map_dfr(.x = arquivos_raspados,
                 .f = parser_do_jogo,
                 .id = 'game_id')

plan(sequential)
jogos

## ajustando a coluna de id do jogo
jogos <- jogos %>% 
  mutate(
    # pegando apenas os digitos do game_id
    game_id = str_extract(string = game_id, pattern = '[0-9]+'),
    # parseando o game_id para numerico
    game_id = parse_number(x = game_id),
    # clonando o game_id para outra coluna
    id_original = game_id,
    # adicionando flag para indicar que nao sao jogos de expansao
    eh_expansao = 'nao',
    # adicionando flag para indicar se o jogo tem expansao
    tem_expansao = map_lgl(.x = tbl_expansion, .f = ~ !is.null(.x))
  )
jogos

# pegando os dados das expansoes ----------------------------------------------------

## pegando os ids das expansoes
ids_das_expansoes <- jogos %>% 
  # pegando a tabela das expansoes de cada game_id
  select(game_id, tbl_expansion) %>% 
  # desaninhando os dados das expansoes
  unnest(cols = tbl_expansion) %>% 
  # pegando apenas as expansoes unicas
  distinct(id_metadado, metadado) %>% 
  # pegando os ids das expansoes
  pull(id_metadado) %>% 
  # parseando para numerico
  parse_number()

## pegando os metadados das expansoes arquivos
walk(.x = ids_das_expansoes,
     .f = pega_jogo_insistente, path_to_save = path_scrapped_expansions)

# parseando as expansoes ------------------------------------------------------------

## listando todos os arquivos que pegamos
expansoes_raspados <- dir_ls(path = path_scrapped_expansions)

## paralelizando parsing usando um safely na funcao para pegar as exceções
plan(multisession)
## parseando os arquivos raspados
expansoes <- map(.x = expansoes_raspados,
                 .f = safely(parser_do_jogo))
plan(sequential)
expansoes

## tratando os dados das expansoes para juntalos
expansoes <- expansoes %>% 
  # redividindo o resultado em duas sublistas: error e result
  transpose() %>% 
  # pegando a lista dos resultados
  pluck('result') %>% 
  # descartando os resultados que nao geraram nenhum tibble
  discard(is.null) %>% 
  # juntando os tibbles linha a linha
  bind_rows(.id = 'game_id') %>% 
  # tratando o id dos jogos e enriquecendo de informacoes
  mutate(
    # pegando apenas os digitos do game_id
    game_id = str_extract(string = game_id, pattern = '[0-9]+'),
    # parseando o game_id para numerico
    game_id = parse_number(x = game_id),
    # adicionando um game_id para especificar que eh expansao
    id_original = game_id,
    # adicionando flag para indicar que sao jogos de expansao
    eh_expansao = 'sim',
    # adicionando flag para indicar se a expansao tem expansao
    tem_expansao = map_lgl(.x = tbl_expansion, .f = ~ !is.null(.x))
  )
expansoes

# juntando os dados de todos os jogos -----------------------------------------------

# juntando os dados dos jogos e expansoes em um dataframe so
df <- bind_rows(jogos, expansoes) %>% 
  # ajustando o game_id dos jogos
  mutate(
    # ajustando o game_id para diferenciar ainda mais as expansoes - existem casos de game_ids duplicados entre
    # jogos normais e jogos expansao
    game_id = ifelse(test = eh_expansao == 'sim', yes = paste0(game_id, '_e'), no = game_id),
    # transformando os game_ids das expansoes em NAs para compatibilizar eles com a tabela de rankings
    id_original = ifelse(test = eh_expansao == 'sim', yes = NA, no = id_original)
  )
df

# tratando os dados -----------------------------------------------------------------

## expandindo cada uma das tabelas que contem multiplas informacoes sobre cada jogo
tabelas <- df %>% 
  # dropando a tabela de comentarios
  select(-tbl_comentarios) %>% 
  # pegando apenas as colunas cujos elementos sejam tibbles com informacoes sobre 
  # alguma dimensao relevante de cada jogo
  select(game_id, contains('tbl')) %>% 
  # passando a base para o formato longo
  pivot_longer(cols = -game_id, names_to = 'tabela', values_to = 'dados') %>% 
  # separando a base em listas de acordo com a dimensao
  split(.$tabela) %>% 
  # desaninhando cada tabela
  map(.f = unnest, cols = dados) %>% 
  # dropando a coluna do id da tabela
  map(.f = select, -tabela) 

## colocando as informacoes da categoria, familia e mecanica de jogo em um formato tidy
tabelas_metadados <- tabelas %>% 
  # removendo apenas a tabela com os nomes de cada titulo
  keep(names(.) %in% c('tbl_category', 'tbl_family', 'tbl_mechanic')) %>% 
  # agrupando todas as tabelas pelo id do jogo
  map(.f = group_by, game_id) %>% 
  # sumarizando todas as informacoes de forma a termos uma linha por jogo
  map(.f = summarise, metadado = paste0(unique(metadado), collapse = ';')) %>% 
  # colocando tudo em uma unica tabela
  bind_rows(.id = 'informacao') %>% 
  # removendo o prefixo tbl
  mutate(informacao = str_remove(string = informacao, pattern = 'tbl_')) %>% 
  # passando a base para o formato largo
  pivot_wider(id_cols = game_id, names_from = informacao, values_from = metadado)

## criando features quantitativas para a quantidade de nomes, artistas e etc que o jogo possui
tabelas_quantitativas <- tabelas %>% 
  # pegando apenas as tabelas que precisaremos sumarizar ao numero de ocorrencias
  keep(names(.) %in% c('tbl_artist', 'tbl_designer', 'tbl_compilation', 'tbl_expansion', 
                       'tbl_implementation', 'tbl_integration', 'tbl_nomes', 'tbl_publisher')) %>% 
  # contando quantas vezes cada game_id aparece - isto representa a quantidade de nomes,
  # artistas e etc que o jogo possui
  map(count, game_id) %>% 
  # juntando todas as listas em um tibble so, colocando o nome da tabela como uma coluna
  bind_rows(.id = 'tabela') %>% 
  # substituindo o padrao 'tbl' por 'n' para indicar a natureza da feature 
  mutate(
    tabela = str_replace(string = tabela, pattern = 'tbl', replacement = 'n')
  ) %>% 
  # passando a base para o formato largo
  pivot_wider(id_cols = game_id, names_from = tabela, values_from = n)

## colocando a tabela com os jogos em um formato tidy
df_tidy <- df %>% 
  # removendo todas as colunas que tenham como prefixo tbl
  select(-starts_with('tbl_'), -rpg) %>% 
  # juntando o nome original do jogo
  left_join(y = tabelas %>% 
              # pegando a tabela com todos os nomes dos jogos
              pluck('tbl_nomes') %>% 
              # pegando apenas o nome original
              filter(metadado == 'primary') %>% 
              # dropando o identificador do tipo de nome do jogo
              select(-metadado),
            by = 'game_id') %>% 
  # adicionando a base com os dados de categoria, mecanica e familia no formato tidy
  left_join(y = tabelas_metadados, by = 'game_id') %>% 
  # adicionando a base com os dados quantitativos sumarizados de cada jogo
  left_join(y = tabelas_quantitativas, by = 'game_id') %>% 
  # adicionando a base das tabelas com a informacao da idade recomendada no formato tidy
  left_join(y = tabelas %>% 
              # pegando a tabela com os resultados da votacao da idade recomendada do jogo
              pluck('tbl_votacao_idade') %>% 
              # removendo todas as opcoes que nao receberam votos
              filter(num_votos != 0) %>% 
              # agrupando pelo id do jogo
              group_by(game_id) %>% 
              # filtrando a opcao que recebeu mais votos
              filter(num_votos == max(num_votos)) %>% 
              # pegando a primeira opcao de cada game_id, garantindo que tenhamos apenas
              # um resultado por jogo
              slice_head(n = 1) %>% 
              # desagrupando a tabela
              ungroup %>% 
              # dropando a coluna da quantidade de votos
              select(-num_votos), 
            by = 'game_id') %>% 
  # adicionando a base das tabelas com a informacao da quantidade de jogadores no formato tidy
  left_join(y = tabelas %>% 
              # pegando a tabela com os resultados da votacao da quantidade recomendada de jogadores
              pluck('tbl_votacao_jogadores') %>% 
              # removendo todas as opcoes que nao receberam votos
              filter(num_votos != 0) %>% 
              # agrupando pelo id do jogo e recomendacao
              group_by(game_id, voto) %>% 
              # filtrando a opcao que recebeu mais votos
              filter(num_votos == max(num_votos)) %>% 
              # pegando a primeira opcao de cada game_id, garantindo que tenhamos apenas
              # um resultado por jogo
              slice_head(n = 1) %>% 
              # desagrupando a tabela
              ungroup %>% 
              # dropando a coluna da quantidade de votos
              select(-num_votos) %>% 
              # passando a base para o formato largo
              pivot_wider(id_cols = game_id, names_from = 'voto', values_from = 'num_jogadores') %>% 
              # renomeando as colunas da base resultante
              set_names(nm = c('game_id', 'melhor_n_jogadores', 'pior_n_jogadores', 'recomendado_n_jogadores')),
            by = 'game_id') %>% 
  # adicionando a base das tabelas com a informacao da dependencia do idioma no formato tidy
  left_join(y = tabelas %>% 
              # pegando a tabela com os resultados da votacao da dependencia de idioma
              pluck('tbl_votacao_idioma') %>% 
              # removendo todas as opcoes que nao receberam votos
              filter(num_votos != 0) %>% 
              # agrupando pelo id do jogo
              group_by(game_id) %>% 
              # filtrando a opcao que recebeu mais votos
              filter(num_votos == max(num_votos)) %>% 
              # pegando a primeira opcao de cada game_id, garantindo que tenhamos apenas
              # um resultado por jogo
              slice_head(n = 1) %>% 
              # desagrupando a tabela
              ungroup %>% 
              # dropando a coluna da quantidade de votos
              select(-num_votos), 
            by = 'game_id') %>% 
  # colocando o titulo do jogo depois do id
  relocate(titulo, .after = game_id) %>% 
  # colocando as informacoes do numeroc de jogadores juntos
  relocate(melhor_n_jogadores, pior_n_jogadores, recomendado_n_jogadores, .after = maxplayers) %>% 
  # colocando informacoes da idade juntos
  relocate(idade_ideal, .after = minage) %>% 
  # renomeando as colunas
  rename(notas = usersrated, nota_usuarios = average, nota_bgg = bayesaverage,
         possuem = owned, trocam = trading, querem = wanting, desejam = wishing,
         ano_publicacao = yearpublished, categoria = category,
         familia = family, mecanica = mechanic)
df_tidy

# salvando as tabelas ---------------------------------------------------------------

## base com os dados apenas das colunas com as informacoes mais simples
write_rds(x = select(df, where(~ !is.list(.x))), file = '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/metadados_parciais_dos_jogos_bgg_.rds')
## base com os dados no formato tidy
write_rds(x = df_tidy, file = '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/metadados_dos_jogos_bgg.rds')
## salvando cada uma das tabelas referentes às list-columns com tibbles
map(
  .x = names(tabelas), 
  .f = ~ write_rds(x = pluck(tabelas, .x), file = paste0('_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/', .x, '_bgg.rds'))
)
