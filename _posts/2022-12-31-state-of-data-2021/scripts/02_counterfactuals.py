###################################
#      CARREGANDO OS PACOTES      #
###################################

import pandas as pd
import numpy as np
import dice_ml
import json
from dice_ml import Dice
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import make_column_transformer
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split, StratifiedKFold, GridSearchCV
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix

###################################
#       CARREGANDO OS DADOS       #
###################################

df = pd.read_csv('_posts/2022-12-31-state-of-data-2021/data/outputs/base_analitica.csv')

###################################
#       SEPARANDO OS DADOS        #
###################################

## dropando a coluna de id
df.drop(columns = ['P0'], inplace = True)

## separando o target das features
X, y = df.drop(columns = ['P2_g']), df.loc[:, 'P2_g']

## pegando as colunas categoricas
colunas_categoricas = X.dtypes[X.dtypes != "int64"].index.values.tolist()
colunas_numericas = X.dtypes[X.dtypes == "int64"].index.values.tolist()

## separando a base de treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 42, shuffle = True, stratify = y)

###################################
#        TREINANDO O MODELO       #
###################################

## criando o pipeline
# instanciando o OneHotEncoding
categorical_processor = make_column_transformer(
  (OneHotEncoder(sparse = False), colunas_categoricas), remainder = 'passthrough'
)
# instanciando o pipeline
pipeline = make_pipeline(
  categorical_processor, 
  StandardScaler(), 
  LogisticRegression(random_state = 42, max_iter = 1000, solver = 'saga', penalty = 'elasticnet')
)

## criando instancia do KFold
skf = StratifiedKFold(n_splits = 10, shuffle = True, random_state = 42)

## criando dicionario de parametros
parametros = {'logisticregression__C': np.logspace(-4, 4, 10), 'logisticregression__l1_ratio': np.linspace(0.001, 0.999, 10)}

## ajustando o gridsearch
cvs = GridSearchCV(estimator = pipeline, param_grid = parametros, cv = skf, scoring = 'neg_log_loss')

## ajustando o grid search
final_model = cvs.fit(X_train, y_train)

## pegando o melhor estimador
cvs.best_estimator_

## pegando as previsoes
previsoes = cvs.predict(X_test)
class_labels = cvs.classes_

## olhando as metricas
cvs.best_score_ # log loss
print(classification_report(y_true = y_test, y_pred = previsoes, labels = class_labels))
confusion_matrix(y_true = y_test, y_pred = previsoes, labels = class_labels)

## pegando os coeficientes
coeficientes = pd.DataFrame(cvs.best_estimator_[-1].coef_, columns = cvs.best_estimator_[0].get_feature_names_out(), index = cvs.classes_)

###################################
#   EXTRAINDO AS INSTÂNCIAS ALVO  #
###################################

## extraindo os erros associados a cada base
X_test_error = X_test.loc[previsoes != y_test, :]
X_train_error = X_train.loc[cvs.predict(X_train) != y_train, :]

## pegando os y dos erros
y_erros = np.append(y_test[previsoes != y_test].values, y_train[cvs.predict(X_train) != y_train].values)
y_hat_erros = np.append(previsoes[previsoes != y_test], cvs.predict(X_train)[cvs.predict(X_train) != y_train])

## concatenando as duas bases
df_erros = pd.concat([X_test_error, X_train_error])

## concatenando os targets à base
df_erros['P2_g'] = y_erros

###################################
#   EXTRAINDO OS COUNTERFACTUALS  #
###################################

## instanciando counterfactuals
d_df = dice_ml.Data(dataframe = df_erros, continuous_features = colunas_numericas, outcome_name = 'P2_g')
d_model = dice_ml.Model(model = final_model, backend = 'sklearn', model_type = 'classifier')
exp_model = Dice(data_interface = d_df, model_interface = d_model, method ='random')

## criando dicionario do nome da classe para o id numerico
class_to_idx = {classe: idx for idx, classe in enumerate(class_labels)}
idx_to_class = {idx: classe for idx, classe in enumerate(class_labels)}

## criando um dataframe para armazenar os resultados de cada rodada do loop
df_counterfactual = pd.DataFrame(columns = ['Subject', 'CF', 'Feature', 'Original', 'Counter', 'Target', 'Prediction'])

## loopando entre cada uma das instâncias em que houve um erro de previsão
for observation, (target, prediction) in enumerate(zip(y_erros, y_hat_erros)):
  # indexando as features relacionadas à instância alvo
  query_instance_example = df_erros.iloc[observation:observation + 1, :-1]
  # gerando 100 exemplos contra-factuais que levariam o modelo a acertar o nivel
  query_counterfactuals = exp_model.generate_counterfactuals(query_instance_example, total_CFs = 100, desired_class = class_to_idx[y_erros[0]])
  # parseando o resultado dos contra-factuais para um arquivo JSON - isso fará com que a extração dos resultados fique mais facil
  resultado = json.loads(query_counterfactuals.to_json())
  # extraindo os nomes das features na mesma ordem que elas estão no JSON
  feature_names_in = resultado['feature_names_including_target']
  # interando entre cada um dos exemplos contra-factuais para a instância em questão
  for idx, counterfactual in enumerate(resultado['cfs_list'][0]):
    # identificação as features que precisam mudar para levar à alteração desejada na previsão - criando uma máscara booleana
    # para definir quais delas são
    diff_mask = [a != b for a,b in zip(resultado['test_data'][0][0], counterfactual)]
    # extraindo os valores originais das features que precisam mudar
    original = np.array(resultado['test_data'][0][0])[diff_mask][:-1]
    # extraindo os valores para os quais estas features devem mudar para que a previsão seja correta
    counterf = np.array(counterfactual)[diff_mask][:-1]
    # pegando o nome das features que precisam mudar
    feats = np.array(feature_names_in)[diff_mask][:-1]
    # criando um dataframe temporário para armazenar essas informações
    temp = pd.DataFrame(columns = ['Subject', 'CF', 'Feature', 'Original', 'Counter', 'Target', 'Prediction'])
    # colocando o nome das features em uma coluna
    temp['Feature'] = feats
    # colocando o valor original das features em uma coluna
    temp['Original'] = original
    # colocando o valor para o qual as features precisam mudar em uma coluna
    temp['Counter'] = counterf
    # adicionando o nivel original do target
    temp['Target'] = target
    # adicionando a previsao que o modelo havia dado
    temp['Prediction'] = prediction
    # adicionando o contador do identificador do contra-factual
    temp['CF'] = idx + 1
    # adicionando o contador do identificador da instância
    temp['Subject'] = observation + 1
    # concantenando os resultados do loop com aqueles existentes
    df_counterfactual = pd.concat([df_counterfactual, temp])
  

###################################
#       SALVANDO OS OUTPUTS       #
###################################

## determinando o path local para escrever
local_path_to_save = '_posts/2022-12-31-state-of-data-2021/data/outputs'

## escrevendo os counterfactuals
df_counterfactual.to_excel(f'{local_path_to_save}/counterfactuals.xlsx', index = False)

## escrevendo os coeficientes
coeficientes.reset_index().to_excel(f'{local_path_to_save}/sklearn_coefficients.xlsx', index = False)
