
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(httr)
library(xml2)

# pegando as funções --------------------------------------------------------------------------------------------------------------------------------------

source(file = 'R/pega_ranking.R')

# pegando paginas do ranking de RPG -----------------------------------------------------------------------------------------------------------------------

walk(.x = 1:2, 
     .f = ~ {
             pega_pagina_ranking(pagina = .x, tipo = 'rpg', silencioso = TRUE, salvar = TRUE, path_salvar = 'data/raw/ranking/')
             Sys.sleep(time = runif(n = 1, min = 0, max = 5))
     }
)

# pegando paginas do ranking the boardgames ---------------------------------------------------------------------------------------------------------------

walk(.x = 1:54, 
     .f = ~ {
             pega_pagina_ranking(pagina = .x, tipo = 'boardgames', silencioso = TRUE, salvar = TRUE, path_salvar = 'data/raw/ranking/')
             Sys.sleep(time = runif(n = 1, min = 0, max = 5))
     }
)

# organizando os dados das paginas extraidas --------------------------------------------------------------------------------------------------------------

## listando todos os arquivos
arquivos <- list.files(path = 'data/raw/ranking', pattern = 'ranking_')
names(arquivos) <- arquivos

## pegando a tabela do ranking
ranking <- map_dfr(.x = arquivos,
                   .f = ~ pega_ranking(arquivo = paste0('data/raw/ranking/', .x)),
                   .id = 'tipo')

# tratando os dados ---------------------------------------------------------------------------------------------------------------------------------------

ranking <- ranking %>% 
        mutate(ano = str_extract(string = ano, pattern = '[0-9]+'),
               ranking = str_extract(string = ranking, pattern = '[0-9]+'),
               nota_rank = str_extract(string = notas, pattern = '(?<=Nota\\sRank\\:\\s)[0-9]{1,2}\\.[0-9]{1,2}'),
               nota_media = str_extract(string = notas, pattern = '(?<=Média\\:\\s)[0-9]{1,2}\\.[0-9]{1,2}'),
               nota_media = str_extract(string = notas, pattern = '(?<=Média\\:\\s)[0-9]{1,2}\\.[0-9]{1,2}'),
               notas = str_extract(string = notas, pattern = '(?<=Notas\\:\\s)[0-9]+'),
               tipo = str_extract(string = tipo, pattern = '(?<=ranking_)([a-z]+)(?=_pagina)')
        ) %>% 
        relocate(notas, .after = nota_media) %>%
        mutate(across(ano:notas, as.numeric))

# exportando os dados -------------------------------------------------------------------------------------------------------------------------------------

write_rds(x = ranking, file = 'data/tidy/ranking.rds')
