
# limpando o ambiente ---------------------------------------------------------------

rm(list = ls(all = TRUE))

# carregando pacotes ----------------------------------------------------------------

library(tidyverse)
library(RSelenium)
library(wdman)

# inicializando o selenium ----------------------------------------------------------

## extraindo o comando que precisamos rodar no terminal para executar o selenium
selCommand <- selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"), 
                       retcommand = TRUE)

## pegar o comando abaixo e jogar no terminal
cat(selCommand)

## criando a instance do driver remoto do selenium na porta que foi aberta
remDr <- remoteDriver(port = 4567L, browserName = 'firefox', 
                      #extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless')))
)

# abrindo o driver ------------------------------------------------------------------

## abrindo uma sessao do web driver
remDr$open()

# entrando no site da calculadora geografica do INPE
remDr$navigate('http://www.dpi.inpe.br/calcula/')

# inputs para o primeiro frame ------------------------------------------------------

# pegando o id do primeiro frame
first_frame <- remDr$findElement(using = 'name', value = 'contents')
# passando para o primeiro frame para preencher 
remDr$switchToFrame(Id = first_frame)
# selecionando a projecao de entrada
proj_in <- remDr$findElement(using = 'xpath', value = '//html/body/div[1]/center/table/tbody/tr[3]/td/select/option[@value="latlong_gd"]')
proj_in$clickElement()
## imputando o valor da longitude
input_X = remDr$findElement(using = 'xpath', value = '/html/body/div[1]/center/table/tbody/tr[5]/td/input')
input_X$sendKeysToElement(sendKeys = list('-43.1034'))
## imputando o valor da latitude
input_Y = remDr$findElement(using = 'xpath', value = '/html/body/div[1]/center/table/tbody/tr[7]/td/input')
input_Y$sendKeysToElement(sendKeys = list('-22.8822'))
# selecionando o datum de entrada
datum_in <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/center/table/tbody/tr[9]/td/select/option[@value="1"]')
datum_in$clickElement()
## clicando no avancar
botao_avancar <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/center/table/tbody/tr[10]/td')
botao_avancar$clickElement()

# inputs para o segundo frame -------------------------------------------------------

## voltando para o frame principal
remDr$switchToFrame(Id = NULL)
# pegando o id do segundo frame
second_frame <- remDr$findElement(using = 'name', value = 'mainp')
# passando para o segundo frame para preencher 
remDr$switchToFrame(Id = second_frame)
# selecionando a projecao de saida
proj_out = remDr$findElement(using = 'xpath', value='/html/body/table/tbody/tr[2]/td/select/option[@value="latlong"]')
proj_out$clickElement()
# selecionando o datum de saida
datum_out = remDr$findElement(using = 'xpath', value='/html/body/table/tbody/tr[4]/td/select/option[@value="5"]')
datum_out$clickElement()
## clicando no avancar
botao_avancar <- remDr$findElement(using = 'xpath', value = '/html/body/table/tbody/tr[5]/td')
botao_avancar$clickElement()

# pegando o resultado ---------------------------------------------------------------

## voltando para o frame principal
remDr$switchToFrame(Id = NULL)
# pegando o id do frame final
last_frame <- remDr$findElement(using = 'name', value = 'canvas')
# passando para o frame final que contem os resultados
remDr$switchToFrame(Id = last_frame)
# pegando a tabela
output <- remDr$findElement(using = 'xpath', value = '/html/body/table/tbody')

## extraindo os dados da tabela e tratando eles
output$getPageSource() %>% 
  # pegando o primeiro elemento d lista
  pluck(1) %>% 
  # lendo o html dele
  rvest::read_html() %>% 
  # parseando a tabela
  rvest::html_table() %>% 
  # tirando a tabela de dentro da lista
  pluck(1) %>% 
  # eliminando o que é lixo
  filter(!X1 %in% c('Resultado', 'Resultado da conversao:', '--'), X2 != '--') %>% 
  # passando a tabela para o formato largo
  pivot_wider(names_from = X1, values_from = X2) %>% 
  # tratando o nome das colunas
  janitor::clean_names() %>% 
  # parseando a longitude e latitude numerica
  mutate(
    longitude_em_gd = parse_number(longitude_em_gd),
    latitude_em_gd  = parse_number(latitude_em_gd)
  )

# fechando o web driver -------------------------------------------------------------

remDr$close()
