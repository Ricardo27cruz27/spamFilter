#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 16:29:13 2019

@author: rolando
"""
#-----------------------------------------------------------------------------#
### LECTURA Y PREPARACION DE DATOS ###
#-----------------------------------------------------------------------------#

#Modificar el directorio de trabajo como la raiz del repositorio spamFilter"
import os

path = "/home/rolando/Dropbox/Semestre201/Cómputo Estadístico/Proyecto/proyecto-git/spamFilter"
os.chdir(path)
os.getcwd()
os.listdir(os.getcwd())

import pandas as pd 
import numpy as np

#nombres de las columnas contenidos en el archivo "var_names.txt"
with open("var_names.txt") as f:
    nombre_col = f.read().splitlines()     

#nombre del xlsx
file = r'datos_spam.xlsx'

#lectura del archivo y creacion del dataframe 
#se eliminan los ultimos dos renglones que son parte del dataframe
df = pd.read_excel(file, header = None)
df.drop([df.index[-2],df.index[-1]], inplace=True)
df.columns = nombre_col

#modificacion de la columna spam como int (por defecto es float al cargar los datos)
df['spam'] = df['spam'].astype(int, copy = False)

#separacion del dataframe en la parte de predictoras (X) y de respuesta (y)
X = df.iloc[:, :-1]
y = df.iloc[:, -1]

#numero de palabras en el vocabulario, está indicado en el README del dataset
n_word_vocab = 48
#seleccion de las columnas que comprenden la matriz de frecuencias de terminos
#de las palabras del vocabulario en cada uno de los archivos de texto
#bw de "Bag of Words"
#X_bw = X.iloc[:, :n_word_vocab]
#X_bw.sum(axis = 1)


#from sklearn.feature_extraction.text import TfidfTransformer
#tfidf = TfidfTransformer(norm='l2', use_idf=True, smooth_idf=True, sublinear_tf = False)
#X_tfidf = tfidf.fit_transform(X_bw.values)
#X
#X.iloc[:, :n_word_vocab] = X_tfidf.toarray()

#-----------------------------------------------------------------------------#
### ANALISIS EXPLORATORIO ###
#-----------------------------------------------------------------------------#


#X
#
#from sklearn.decomposition import PCA
#pca = PCA(n_components=2)
#X_r = pca.fit(X).transform(X)

##
#cambiar la forma de constuir segun el peso
from sklearn.model_selection import train_test_split
help(train_test_split)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)


#numero de ejemplos
np.unique(y_test, return_counts = True)
np.unique(y_train, return_counts = True)


#word cloud
data_fm

#-----------------------------------------------------------------------------#
### CLASIFICACION Y EVALUACION DE MODELOS ###
#-----------------------------------------------------------------------------#

# CLASIFICADORES #
# Decision Tree
from sklearn import tree
# Logistic Regression
from sklearn.linear_model import LogisticRegression
# SVM
from sklearn.svm import LinearSVC
from sklearn.svm import SVC
# Random Forest
from sklearn.ensemble import RandomForestClassifier

# METRICAS #
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report

# VALIDACION CRUZADA #
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
#calcular metricas


clf = tree.DecisionTreeClassifier(criterion = "gini",random_state=0, 
                                  min_samples_leaf = 200,
                                  max_depth = 5,
                                  max_leaf_nodes = 5)

#dt
clf = tree.DecisionTreeClassifier(criterion = "gini", random_state=0, max_depth = 5)
clf = tree.DecisionTreeClassifier(criterion = "gini", random_state=0)

#help(tree.DecisionTreeClassifier)
#se pueden pasar otros parámetros scoring='f1_macro'
#scores = cross_val_score(clf, X_train, y_train, cv=5, scoring='f1')
#scores 

#obtener parametros del estimador
clf.get_params()

#scoring= 'recall'
#ajustar hiperparametros
#puede ser una lista de diccionarios
#falta pasar None
param_grid= {'max_leaf_nodes': list(range(3,21)),
             'max_depth': list(range(2,30)),
             'min_samples_leaf':list(range(1,420, 20))
}

param_grid= {'max_leaf_nodes': list(range(4,21)),
             'max_depth': list(range(3,31)),
             'min_samples_leaf':list(range(1,11))
}

param_grid= {'max_leaf_nodes': [6],
             'max_depth': [20],
             'min_samples_leaf':[1]
}

#6, 20, 1
clf_cv = GridSearchCV(clf, param_grid, cv = 5, verbose = 1)
clf_cv .get_params()
clf_cv.fit(X_train, y_train)
#clf_cv.best_estimator_
clf_cv.best_params_

#tree.plot_tree(clf_cv.best_estimator_)

clf_cv_y_predict = clf_cv.predict(X_test)
confusion_matrix(y_test,clf_cv_y_predict)
clf_cv_X_test_score = clf_cv.score(X_test, y_test)
print('Test Accuracy:', clf_cv_X_test_score)
print(classification_report(y_test, clf_cv_y_predict))


#best_estimator_
#cv_results_
#clf.fit

clf = clf.fit(X_train, y_train)
clf_y_predict = clf.predict(X_test)
confusion_matrix(y_test,clf_y_predict)
clf_X_test_score = clf.score(X_test, y_test)
print('Test Accuracy:', clf_X_test_score)
print(classification_report(y_test, clf_y_predict))

#tree.plot_tree(clf)


#############
import graphviz 
import pydotplus
dot_data = tree.export_graphviz(clf, out_file=None, feature_names=df.columns[:-1],
                                class_names = ["Ham","Spam"], filled=True, 
                                rounded=True, special_characters=True) 
graph = pydotplus.graph_from_dot_data(dot_data)
graph.write_png(os.getcwd()+"/figuras/arbol.png")
#graph = graphviz.Source(dot_data) 

#graph
#############
#from plotlib.pylab import rcParams
import matplotlib
#rcParams['figure.figsize'] = 20,12
#tree.plot_tree(clf)
#fig = matplotlib.pyplot.gcf()
#fig.set_size_inches(20, 10)
#fig.savefig('tree.png')

#Naive Bayes

#LR
lr = LogisticRegression(penalty='l2', max_iter=10000, C=1, random_state=42)
lr.fit(X_train, y_train)
lr_y_predict = lr.predict(X_test)
confusion_matrix(y_test,lr_y_predict)
lr_X_test_score = lr.score(X_test, y_test)
print('Test Accuracy:', lr_X_test_score)
index_sort = np.argsort(-np.abs(lr.coef_.reshape(-1)))
#prueba de hipotesis
coef = pd.DataFrame({"col": X.columns[index_sort ], "coef":lr.coef_.reshape(-1)[index_sort ] })
coef 
#coef 
print(classification_report(y_test, lr_y_predict ))


#SVM
svm = LinearSVC(penalty='l2', C=1, random_state=42, max_iter = 10000)
svm.fit(X_train, y_train)
svm_y_predict = svm.predict(X_test)
confusion_matrix(y_test,svm_y_predict)
svm_X_test_score = svm.score(X_test, y_test)
print('Test Accuracy:', svm_X_test_score)
print(classification_report(y_test, svm_y_predict ))

#
#svc  = SVC(C = 1, kernel = "rbf", max_iter=100000)
#svc.fit(X_train, y_train)
#svc_X_test_score = svm.score(X_test, y_test)
#print('Test Accuracy:', svm_X_test_score)


#RF
rfc = RandomForestClassifier(n_estimators=10, max_depth= 10, min_samples_leaf = 10, random_state=42)
rfc.fit(X_train, y_train)
rfc_y_predict = rfc.predict(X_test)
confusion_matrix(y_test,rfc_y_predict)
rfc_X_test_score = rfc.score(X_test, y_test)
print('Test Accuracy:', rfc_X_test_score)
print(classification_report(y_test, rfc_y_predict))

#-----------------------------------------------------------------------------#
### EVALUACION DE MODELOS CON EL DATASET "ENRON" ###
#https://github.com/singhlaaman/Email-Spam-filtering-with-Enron-Dataset
#-----------------------------------------------------------------------------#

#hacer regularizacion en clasificadores anteriores, seleccion de variables
#Prueba con datos extra

#para obtener nombres de archivos mediante expresiones regulares
import glob
import re

#se obtienen los nombres de los archivos en el directorio y subdirectorios
#path_data = os.getcwd() + '/enron2/ham/*.txt'
#files = glob.glob(path_data)
path_data  = os.getcwd() + '/enron/**/*.txt'
files = glob.glob(path_data , recursive=True)

#listas para guardar los correos y sus etiquetas
data = []
data_label = []

#patrones etiquetar como ham o spam segun el nombre del archivo
#si el nombre del archivo contiene la palabra ham se asigna 0, 
#si contiene la palabra spam se asigna 1
p_ham = ".*ham.*txt"
p_spam = ".*spam.*txt"

#lectura de los archivos
for name in files:
    try:
        with open(name, encoding="utf8", errors='ignore') as f:
            #se asignan etiquetas segun el nombre base del archivo
            if re.search(p_spam, os.path.basename(name)):
                data.append("\n".join(f.readlines()))
                data_label.append(1)
            elif re.search(p_ham, os.path.basename(name)):
                data.append("\n".join(f.readlines()))
                data_label.append(0)
    except IOError as exc:
        if exc.errno != errno.EISDIR:
            raise

#pruebas de lectura
data[0]
data_label[0]

#numero de archivos leidos
len(data)
           
#construccion del vocabulario para obtener la matriz de frecuencias relativas
vocab = []
v_char = []
for c in df.columns:
    l_word = re.findall("word_freq_.*", c)
    l_char = re.findall("char_freq_.*", c)
    if len(l_word) > 0:
        vocab.append(re.findall("word_freq_.*", c)[0].replace("word_freq_", ""))
    if len(l_char) > 0:
        v_char.append(re.findall("char_freq_.*", c)[0].replace("char_freq_", ""))

#palabras del diccionario
vocab
#caracteres a considerar
v_char

#para calcular la matriz de frecuencias absolutas
from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(vocabulary = vocab)
tokenizer = vectorizer.build_tokenizer()
data_cv = vectorizer.transform(data).toarray()

#analizar de forma estadistica la distribucion de las longitudes
#en relacion a la variable de clase, pensar en otras variables que ayuden a discriminar
#usar medidas estadisticas
#convertir a frecuencias relativas, se calcula el numero de palabras por documento
lon_docs = [len(tokenizer(doc)) for doc in data]

#matriz de frecuencias relativas
data_fm = np.apply_along_axis(func1d = lambda x: 100*(x/lon_docs), axis = 0,  arr = data_cv)
#se crea la matriz con los predictores para la clasificacion
X_data = np.zeros((len(data), X.shape[1]))
X_data[:,:n_word_vocab] = data_fm

#matriz de frecuencias caracteres
data_char =  np.zeros((len(data), len(v_char)), dtype = float)

for ind_doc, doc in enumerate(data):
    for ind_char, char in enumerate(v_char):
        data_char[ind_doc, ind_char] = 100*(doc.count(char)/len(doc))

X_data[:,n_word_vocab:n_word_vocab + len(v_char)] = data_char

#matriz de datos de mayusculas
pattern_capital = "[A-Z]*"

n_capital_vars = 3
data_capital =  np.zeros((len(data), n_capital_vars), dtype = float)
for ind_doc, doc in enumerate(data):
    #longitudes de las secuencias de mayusculas
    longitudes = [len(match) for match in re.findall(pattern_capital, doc) if len(match) > 0]
    data_capital[ind_doc, :] = [np.mean(longitudes), np.max(longitudes), np.sum(longitudes)]


#las ultimas 3 columnas son para estas variables
#empeora las predicciones
X_data[:,-n_capital_vars:] = data_capital
#X_data[:,-n_capital_vars:] = 0


#X_data.shape
#variables de secuencias

#el mejor es regresion logística
#recalcar que es solo valido para el contexto del problema, no es escalable
#enron2: clasifica bien el ham, en spam, lr es la mejor
#rfc clasificar mejor la clase negativa
#la clase spam
#se puede hacer un ensamble?
#lr clasifica mejor la clase positiva
#con tf idf se obtienen mejores resultados en ambos casos, justificar su uso
#se puede hacer una normalizacion ad-hoc 
#hacer enfasis en las metricas usadas
#comparar palabras usadas
#la conclusion es que el problema no es escalable
#para correos que no pertenecen al dataset
#en parte es porque la matriz de terminos es muy pequeña
#es suficiente para el dataset con el que se construyo, 
#que puede ser muy especifico en el contexto (hp, negocios, ver la fuente)
#revisar papers
np.unique(data_label , return_counts = True)

rfc_y_predict = rfc.predict(X_data)
rfc_X_test_score = rfc.score(X_data, data_label)
print('Test Accuracy:', rfc_X_test_score)
print(classification_report(data_label, rfc_y_predict))
confusion_matrix(data_label,rfc_y_predict)
np.unique(rfc_y_predict , return_counts = True)

lr_y_predict = lr.predict(X_data)
lr_X_test_score = lr.score(X_data, data_label)
print('Test Accuracy:', lr_X_test_score)
print(classification_report(data_label, lr_y_predict ))
confusion_matrix(data_label,lr_y_predict)
np.unique(lr_y_predict , return_counts = True)

svm_X_test_score = svm.score(X_data, data_label)
svm_y_predict = svm.predict(X_data)
print('Test Accuracy:', svm_X_test_score)
print(classification_report(data_label, svm_y_predict ))
confusion_matrix(data_label,svm_y_predict)
np.unique(svm_y_predict , return_counts = True)

clf_cv_y_predict = clf_cv.predict(X_data)
clf_cv_X_test_score = clf_cv.score(X_data, data_label)
print('Test Accuracy:', clf_cv_X_test_score)
print(classification_report(data_label, clf_cv_y_predict))
confusion_matrix(data_label,clf_cv_y_predict)
np.unique(clf_cv_y_predict, return_counts = True)



#-----------------------------------------------------------------------------#
### VISUALIZACION, PAPER WORD CLOUD? ###
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
### EXTRA ###
#-----------------------------------------------------------------------------#

s = "3d esta es tu oportunidad!quieres ganar mucho dinero?? 100,000"

#hacer cbind para tener un dataframe de los datos leídos
#tokenizer = vectorizer.build_tokenizer()
tokenizer(s)
vectorizer.transform(data_spam)
#####################################

re.findall("_freq_.*", df.columns[2])
re.findall("_freq_.*", df.columns[-7])
c = df.columns[2]
c = df.columns[-7]
re.findall("_freq_\w*", c)
re.findall("_freq_.*", c)
re.findall("_freq_*\W*", c)

re.findall("_freq_.*", c)
data_tf_matrix = np.zeros((len(data_spam), len(vocab)), dtype = int)
len(vocab)

for ind_doc, doc in enumerate(data_spam):
    for ind_word, word in enumerate(vocab):
        data_tf_matrix[ind_doc, ind_word] = doc.count(word)

data_tf_matrix

data_tf_matrix.shape
data_tfidf = tfidf.transform(data_tf_matrix)

X_tfidf = tfidf.fit_transform(X_bw.values)


from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(vocabulary = vocab)
tokenizer = vectorizer.build_tokenizer()
s = "3d esta es tu oportunidad!quieres ganar mucho dinero??"

s.split()
s.count("!")
tokenizer(s)
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#





import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
#PCA
pca = PCA(n_components=2)
X_r = pca.fit(X).transform(X)

#--------------------------------#
plt.figure()
colors = ['navy', 'turquoise']
lw = 2

X_r[0,0]
for color, i, target_name in zip(colors, [0, 1], [0,1]):
    plt.scatter(X_r[y == i, 0], X_r[y == i, 1], color=color, alpha=.8, lw=lw,
                label=target_name)
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('PCA of IRIS dataset')

#--------------------------------#
df.drop(df.shape[0]-1)
df[0]
df.loc[df.shape[0]-1]

df.shape[0]

?pd.read_excel 





#aplicar tecnicas de kernel