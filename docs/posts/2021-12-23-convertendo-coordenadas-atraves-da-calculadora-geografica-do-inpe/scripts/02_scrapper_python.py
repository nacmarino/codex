from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import re

##########################################################################################
#                                    ABRINDO O DRIVER                                    #
##########################################################################################

# setando as opcoes para rodar headless
options = Options()
options.headless = True
# abrindo o driver do Chrome
driver = webdriver.Chrome(executable_path='/Users/Nicholas/Downloads/chromedriver', options = options)
# entrando no site da calculadora geografica do INPE
driver.get(url = 'http://www.dpi.inpe.br/calcula/')
# aguardando o site carregar
driver.implicitly_wait(10)

##########################################################################################
#                             INPUTS PARA O PRIMEIRO FRAME                               #
##########################################################################################

# passando para o primeiro frame para preencher 
driver.switch_to.frame(frame_reference = 'contents')
# selecionando a projecao de entrada
seletor_projecao_entrada = Select(driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[3]//select'))
seletor_projecao_entrada.select_by_value(value = 'latlong_gd')
## imputando o valor da longitude
input_X = driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//td//input')
input_X.send_keys('-43.1034')
## imputando o valor da latitude
input_Y = driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//td//input')
input_Y.send_keys('-22.8822')
# selecionando o datum de entrada
seletor_datum_entrada = Select(driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[9]//select'))
seletor_datum_entrada.select_by_value(value = '1')
## clicando no avancar
driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[10]//td').click()

##########################################################################################
#                              INPUTS PARA O SEGUNDO FRAME                               #
##########################################################################################

# passando para o segundo frame
driver.switch_to.parent_frame()
driver.switch_to.frame(frame_reference = driver.find_element_by_name('mainp'))

# selecionando a projecao de saida
seletor_projecao_saida = Select(driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[2]//select'))
seletor_projecao_saida.select_by_value(value = 'latlong')
# selecionando o datum de saida
seletor_datum_entrada = Select(driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[4]//td//select'))
seletor_datum_entrada.select_by_value(value = '5')
## clicando no avancar
driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[5]//td').click()

##########################################################################################
#                           COLETANDO OUTPUTS DO ULTIMO FRAME                            #
##########################################################################################

# passando para o ultimo frame
driver.switch_to.parent_frame()
driver.switch_to.frame(frame_reference = driver.find_element_by_name('canvas'))

# criando dicionario para armazenar os resultados
outputs = {}

# iterando entre as linhas da tabela
for idx, linha in enumerate(driver.find_elements(by = 'xpath', value='//html//body//table//tbody//tr')):
  # pegando apenas os elementos que estao nas linhas do output que queremos
  if idx in [5, 6, 7, 9, 10, 11]:
    # parseando o resultado para uma string
    resultado = BeautifulSoup(markup = linha.get_attribute('innerHTML'), features = 'html.parser').text.strip()
    # separando o string do valor
    resultado = re.split('\s{2,}', resultado)
    # colocando os resultado no dicionario
    outputs[resultado[0]] = resultado[1]
  else:
    pass
  
# retornando o dicionario
outputs
