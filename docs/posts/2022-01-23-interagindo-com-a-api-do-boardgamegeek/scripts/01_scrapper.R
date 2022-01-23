
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
path_scrapped_data <- '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/temporario'

## criando pasta se ela nao existir
if(!dir_exists(path_scrapped_data)){
  dir_create(path = path_scrapped_data, recurse = TRUE)
}

# definindo funcoes -----------------------------------------------------------------

## funcao para pegar o XML de um jogo
pega_jogo <- function(game_id, path_to_save = NULL) {
  # url base para pegar um jogo, as estatisticas e a primeira pagina de comentarios
  base_url <- str_glue('https://www.boardgamegeek.com/xmlapi2/thing?id={game_id}&stats=1&ratingcomments=1&pagesize=100')
  
  # fazendo o request e salvando o codigo da resposta se o path nao for nulo
  if(!is.null(path_to_save)){
    GET(url = base_url, 
        write_disk(path = sprintf(fmt = '%s/%08d.html', path_to_save, game_id), 
                   overwrite = TRUE)
    )
  } else {
    GET(url = base_url)
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
  arquivo_xml %>% 
    # extrai todas as tags link
    xml_find_all(xpath = '*//link') %>% 
    # extrai todo os atributos dessas tags
    xml_attrs() %>% 
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
      posicao         = parse_number(x = posicao),
      media_bayesiana = parse_number(x = media_bayesiana)
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
  # pega o arquivo html
  arquivo_xml %>% 
    # pega tudo o que está sobre a tag comments
    xml_find_all(xpath = '*/comments') %>% 
    # pega apenas o valor correspondente ao total de comentarios
    xml_attr('totalitems') %>% 
    # parseando o string para numero
    parse_number()
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

# funcao para parsear o arquivo xml inteiro do jogo
parser_do_jogo <- function(path_arquivo_xml) {
  
  ## lendo o arquivo xml
  xml_do_jogo <- read_xml(x = path_arquivo_xml)
  
  ## parseando o arquivo xml
  tibble(
    tbl_nomes             = list(parser_nome(arquivo_xml = xml_do_jogo)),
    tbl_comentarios       = list(parser_comentarios(arquivo_xml = xml_do_jogo)), 
    tbl_rankings          = list(parser_rankings(arquivo_xml = xml_do_jogo)), 
    tbl_votacao_idade     = list(parser_votacao_idade(arquivo_xml = xml_do_jogo)), 
    tbl_votacao_idioma    = list(parser_votacao_idioma(arquivo_xml = xml_do_jogo)),
    tbl_votacao_jogadores = list(parser_votacao_n_jogadores(arquivo_xml = xml_do_jogo))
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
  # selecionando os 10 jogos com mais votos
  top_n(n = 10, wt = votos) %>% 
  # pegando o id deles
  pull(id) %>% 
  # parseando para numerico
  parse_number()

## pegando estes arquivos
map(.x = ids_para_pegar,
    .f = pega_jogo, path_to_save = path_scrapped_data)

# parseando os jogos raspados -------------------------------------------------------

## listando todos os arquivos que pegamos
arquivos_raspados <- dir_ls(path = path_scrapped_data)

## parseando os arquivos raspados
jogos <- map_dfr(.x = arquivos_raspados,
                 .f = parser_do_jogo,
                 .id = 'game_id') %>% 
  mutate(
    # extraindo o game_id a partir do path para o arquivo
    game_id = path_file(path = game_id),
    game_id = path_ext_remove(path = game_id),
    # tirando os zeros do inicio para deixar o game_id dessa tabela igual ao da tabela de ranking
    game_id = str_replace(string = game_id, pattern = '^0+' , replacement = '')
  )
jogos

# exemplo de tratamento dos dados ---------------------------------------------------

## expandindo cada uma das tabelas que contem multiplas informacoes sobre cada jogo
tabelas <- jogos %>% 
  # pegando apenas as colunas cujos elementos sejam tibbles com informacoes sobre 
  # alguma dimensao relevante de cada jogo
  select(game_id, tbl_nomes, tbl_category:tbl_publisher) %>% 
  # passando a base para o formato longo
  pivot_longer(cols = -game_id, names_to = 'tabela', values_to = 'dados') %>% 
  # separando a base em listas de acordo com a dimensao
  split(.$tabela) %>% 
  # desaninhando cada tabela
  map(.f = unnest, cols = dados) %>% 
  # dropando a coluna do id da tabela
  map(.f = select, -tabela) %>% 
  # ordenando as tabelas por jogo em ordem alfabetica do metadado
  map(.f = arrange, game_id, metadado)

## colocando as tabelas no formato tidy
tabelas_tidy <- tabelas %>% 
  # removendo apenas a tabela com os nomes de cada titulo
  discard(names(.) == c('tbl_nomes')) %>% 
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
  
## colocando a tabela com os jogos em um formato tidy
jogos_tidy <- jogos %>% 
  mutate(
    # extraindo nome oficial do jogo
    titulo  = map_chr(.x = tbl_nomes, 
                      .f = ~ filter(.x, metadado == 'primary') %>% pull(titulo)
    ),
    # codificando se o titulo é uma expansao ou nao
    eh_expansao = map2_int(.x = tbl_expansion, .y = titulo, .f = ~ any(.y %in% .x$metadado)),
    # pegando melhor idade
    idade_ideal = map_chr(.x = tbl_votacao_idade, 
                          .f = ~ filter(.x, num_votos == max(num_votos)) %>% 
                            pull(idade_ideal)
    ),
    # pegando melhor numero de jogadores
    melhor_n_jogadores = map_chr(.x = tbl_votacao_jogadores,
                                 .f = ~ filter(.x, voto == 'Best') %>% 
                                   filter(num_votos == max(num_votos)) %>% 
                                   pull(num_jogadores)
    ),
    # pegando pior numero de jogadores
    pior_n_jogadores = map_chr(.x = tbl_votacao_jogadores,
                               .f = ~ filter(.x, voto == 'Not Recommended') %>% 
                                 filter(num_votos == max(num_votos)) %>% 
                                 pull(num_jogadores)
    ),
    # pegando numero de jogadores recomendado
    recomendado_n_jogadores = map_chr(.x = tbl_votacao_jogadores,
                                      .f = ~ filter(.x, voto == 'Recommended') %>% 
                                        filter(num_votos == max(num_votos)) %>% 
                                        pull(num_jogadores)
    ),
    # pegando dependencia do idioma
    dependencia_do_idioma = map_chr(.x = tbl_votacao_idioma,
                                    .f = ~ filter(.x, num_votos == max(num_votos)) %>% 
                                      pull(voto)
    )
  ) %>% 
  # removendo todas as colunas que tenham como prefixo tbl
  select(-starts_with('tbl_')) %>% 
  # adicionando a base das tabelas no formato tidy
  left_join(y = tabelas_tidy, by = 'game_id') %>% 
  # colocando o titulo do jogo depois do id
  relocate(titulo, .after = game_id) %>% 
  # colocando as informacoes do numeroc de jogadores juntos
  relocate(melhor_n_jogadores, pior_n_jogadores, recomendado_n_jogadores, .after = maxplayers) %>% 
  # colocando informacoes da idade juntos
  relocate(idade_ideal, .after = minage) %>% 
  # renomeando as colunas
  rename(notas = usersrated, nota_usuarios = average, nota_bgg = bayesaverage,
         possuem = owned, trocam = trading, querem = wanting, desejam = wishing,
         ano_publicacao = yearpublished, artista = artist, categoria = category,
         edicoes_especiais = compilation, expansoes = expansion, familia = family,
         jogos_base = implementation, mecanica = mechanic, editora = publisher)
jogos_tidy

# salvando as tabelas ---------------------------------------------------------------

## base com os dados brutos apos o parser
write_rds(x = jogos, file = '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/jogos_brutos.rds')
## base com um exemplo dos dados no formato tidy
write_rds(x = jogos_tidy, file = '_posts/2022-01-23-interagindo-com-a-api-do-boardgamegeek/data/jogos_tidy.rds')
