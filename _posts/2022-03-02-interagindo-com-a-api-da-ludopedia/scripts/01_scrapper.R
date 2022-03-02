
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -------------------------------------------------------------

library(tidyverse)
library(httr)
library(fs)

# definindo funcoes -----------------------------------------------------------------

# função para fazer a autenticacao do aplicativo
pegar_access_token <- function(APP, APP_ID, URIs) {
  
  # setando o endpoint de acordo com os dados fornecidos na ludopedia
  ludopedia_endpoint <- httr::oauth_endpoint(request = NULL,
                                             authorize = 'https://ludopedia.com.br/oauth/',
                                             access = 'https://ludopedia.com.br/tokenrequest/'
  )
  
  # setando as configurações do aplicativo conforme definido no site da ludopedia
  ludopedia_app <- httr::oauth_app(
    appname = APP,
    key = APP_ID,
    secret = NULL,
    redirect_uri = URIs
  )
  
  # pegando a autorizacao através do OAUTH2.0
  autorizacao <- httr::init_oauth2.0(endpoint = ludopedia_endpoint, 
                                     app = ludopedia_app)
  
  # criando string do token
  access_token <- paste0(autorizacao$token_type, ' ', autorizacao$access_token)
  
  # retornando o token de acesso
  return(access_token)
}

# função para pegar a quantidade total de jogos disponiveis
pegar_total_jogos <- function(access_token) {
  
  httr::GET('https://ludopedia.com.br/api/v1/jogos', 
            httr::add_headers(AUTHORIZATION = access_token)) %>% 
    httr::content() %>% 
    purrr::pluck('total') %>% 
    as.integer()
  
}

# função para pegar todos os jogos em uma dada página
pegar_uma_pagina <- function(pagina, access_token, path_salvar = NULL) {
  
  # pegando os jogos da pagina selecionada
  jogos_da_pagina <- httr::GET('https://ludopedia.com.br/api/v1/jogos', 
                               httr::add_headers(AUTHORIZATION = access_token),
                               query = list(page = pagina, rows = 100)) %>% 
    httr::content(simplifyDataFrame = TRUE) %>% 
    purrr::pluck('jogos') %>% 
    tibble::as_tibble()
  
  # salvando os dados caso necessario
  if(!is.null(path_salvar)){
    readr::write_rds(x = jogos_da_pagina, file = stringr::str_glue(path_salvar, 'lista_jogos_pagina_{pagina}.rds'))
  }
  
  return(jogos_da_pagina)
  
}

# funcao para pegar as informações de um jogo
informacao_do_jogo <- function(id_jogo, access_token, path_salvar = NULL) {
  
  # pegando os detalhes do jogo
  lista_de_detalhes <- httr::GET(url = stringr::str_glue('https://ludopedia.com.br/api/v1/jogos/{id_jogo}'), 
                                 add_headers(AUTHORIZATION = access_token)) %>% 
    httr::content(simplifyDataFrame = TRUE) %>% 
    tibble::enframe()
  
  # organizando todas as informacoes que não são listas ou dataframes
  vetores <- lista_de_detalhes %>% 
    dplyr::filter(!purrr::map_lgl(value, is.list)) %>% 
    dplyr::mutate(value = ifelse(test = purrr::map_lgl(value, is.null), yes = NA, value),
                  value = purrr::flatten_chr(value)) %>% 
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  # organizando todas as informacoes que sao listas ou dataframes
  dfs <- lista_de_detalhes %>% 
    dplyr::filter(map_lgl(value, is.list)) %>% 
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  # juntando os dois objetos
  detalhes <- dplyr::bind_cols(vetores, dfs)
  
  # salvando a tabela caso necessario
  if(salvar){
    readr::write_rds(x = detalhes, file = stringr::str_glue(path_salvar, 'info_do_jogo_{id_jogo}.rds'))
  }
  
  # retornando a tabela caso necessario
  if(retorna_df){
    return(detalhes)
  }
  
}

pega_pagina_ranking <- function(pagina, tipo = 'boardgames', silencioso = FALSE, salvar = FALSE, path_salvar = NULL) {
  
  # pegando o conteudo da pagina se for boardgame
  if (tipo == 'boardgames') {
    # se for salvar
    if(salvar) {
      conteudo_da_pagina <- GET(url = paste0('https://www.ludopedia.com.br/ranking?pagina=', pagina),
                                write_disk(path = stringr::str_glue(path_salvar, 'ranking_{tipo}_pagina_{pagina}.html'),
                                           overwrite = TRUE)
      )
      # se nao for salvar
    } else {
      conteudo_da_pagina <- GET(url = paste0('https://www.ludopedia.com.br/ranking?pagina=', pagina))
    }
    
    # pegando o conteudo da pagina se for rpg
  } else if (tipo == 'rpg') {
    # se for salvar
    if(salvar) {
      conteudo_da_pagina <- GET(url = paste0('https://www.ludopedia.com.br/ranking?tipo=rpg&pagina=', pagina),
                                write_disk(path = stringr::str_glue(path_salvar, 'ranking_{tipo}_pagina_{pagina}.html'),
                                           overwrite = TRUE)
      )
      # se não for salvar
    } else {
      conteudo_da_pagina <- GET(url = paste0('https://www.ludopedia.com.br/ranking?tipo=rpg&pagina=', pagina))
    }
    
    # caso o argumento tipo esteja errado
  } else {
    print('O argumento tipo deve ser "boardgames" ou "rpg".')
    break
  }
  
  # retorna a resposta caso o usuario deseje
  if(!silencioso){
    return(conteudo_da_pagina)
  }
  
}

# raspando dados --------------------------------------------------------------------

## criando um token de acesso para a sessao
meu_token <- pegar_access_token(APP = 'meu_app', APP_ID = Sys.getenv('app_id'), URIs = 'http://localhost:1410/')

## pegando uma pagina
jogos_da_pagina <- httr::GET('https://ludopedia.com.br/api/v1/jogos', 
                             httr::add_headers(AUTHORIZATION = meu_token))

## olhando o resultado da pagina
jogos_da_pagina %>% 
  content(simplifyDataFrame = TRUE)
