# importando módulos e funções do sklearn
## core
import pandas as pd
import numpy as np
## split da base e validação cruzada
from sklearn.model_selection import train_test_split, GridSearchCV, StratifiedKFold
## pre-processamento
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.feature_selection import VarianceThreshold
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
## ajuste e avaliação do modelo
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import log_loss, roc_auc_score, confusion_matrix

# ----------------------------------------------------------------------- #
#                  Separando os dados da base analítica                   #
# ----------------------------------------------------------------------- #

# pegando a base que está no R e trazendo ela para o Python como uma cópia
df_python = r.df_base_analitica.copy()

# colocando o identificador único de cada pessoa respondente como o índice da base
df_python.set_index(keys = 'P0', inplace = True)

# separando as features do target e usando um split 80:20 para separar a base 
# analítica entre treino e teste
X, y = df_python.drop(columns = ['P2_g']), df_python.loc[:, 'P2_g']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 33, shuffle = True, stratify = y)

# ----------------------------------------------------------------------- #
#                           Criando um pipeline                           #
# ----------------------------------------------------------------------- #

# separando a lista de features que possuem multiplas categoricas daquelas que já estão como dummy
features_categoricas = X.dtypes[X.dtypes == "object"].index.values.tolist()
features_dummy = X.dtypes[X.dtypes != "object"].index.values.tolist()

# consolidando o pipeline de pre-processamento com uma etapa através da qual o one hot enconding é 
# feito, seguido da  remoção de features com variância muito baixa (i.e., a maioria delas é 0 ou 1
# - no nosso caso isso o threshold selecionado equivale à cerca de 20 observações pertencentes à uma
# classe e as outras 390 à outra classe), padronização de todas as features (mesmo as categórica - ver
# Gelman et al) 
pre_processamento = Pipeline(
  steps = [
    ('encoding', ColumnTransformer([
      ('ohe', OneHotEncoder(sparse = False), features_categoricas), 
      ('dummy', 'passthrough', features_dummy)
      ])
      ),
    ('nzv', VarianceThreshold(threshold = 0.03)),
    ('scaler', StandardScaler())
    ]
  )
  
  ColumnTransformer([('ohe', OneHotEncoder(sparse = False), features_categoricas), ('dummy', 'passthrough', features_dummy)])
  
# concluindo o pipeline com a etapa de pre-processamento e o ajuste da regressão logística multiclasse
# usando uma penalidade do elasticnet
pipeline = Pipeline(
  steps = [
    ('pre_processamento', pre_processamento),
    ('modelo', LogisticRegression(random_state = 42, max_iter = 2000, solver = 'saga', penalty = 'elasticnet'))
    ]
  )

# ----------------------------------------------------------------------- #
#                   Buscando hiperparâmetros do modelo                    #
# ----------------------------------------------------------------------- #

## criando instancia do KFold
skf = StratifiedKFold(n_splits = 5, shuffle = True, random_state = 42)

## criando dicionario de parametros
parametros = {'modelo__C': np.logspace(-4, 4, 10), 'modelo__l1_ratio': np.linspace(0.001, 0.999, 10)}

## ajustando o gridsearch
grid_search = GridSearchCV(estimator = pipeline, param_grid = parametros, cv = skf, scoring = 'neg_log_loss')

## ajustando o grid search
grid_search.fit(X_train, y_train)

## pegando o melhor estimador
grid_search.best_estimator_
grid_search.best_score_

confusion_matrix(y_true = y_test, y_pred = grid_search.predict(X_test))
roc_auc_score(y_test, grid_search.predict_proba(X_test), multi_class  = 'ovo')
roc_auc_score(y_test, grid_search.predict_proba(X_test), multi_class  = 'ovr')

pipeline[:-1].get_feature_names_out()
pipeline[-1].coef_
