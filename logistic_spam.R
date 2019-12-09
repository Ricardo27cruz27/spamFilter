### FILTRO DE SPAM ###
#paquetes necesarios
library(sqldf)
library(LaplacesDemon)
library(corrplot)
library(caret)
#funciones utilizadas en el código
densidades<-function(modelo,dataset,cutoff){
  #Probabilidades predichas
  probas<-predict(modelo,dataset,type="response")
  #Probs predichas por categoria real
  prob_sr<-probas[which(dataset$spam==1)]
  prob_nsr<-probas[which(dataset$spam==0)]
  #Densidades de spam y no spam predichas
  den_prob_sr<-density(prob_sr,from = 0,to=1)
  den_prob_nsr<-density(prob_nsr,from=0,to=1)
  #Punto de corte
  if(cutoff==0){
    obs_min<-which.min(abs(den_prob_sr$y-den_prob_nsr$y))
    cutoff<-den_prob_nsr$x[obs_min]
  }
  #plot
  x11()
  plot(den_prob_sr,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="red")
  par(new=T)
  plot(den_prob_nsr,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="darkblue")
  abline(v=cutoff,col="green")
  legend(x="topright",legend=c("dist spam=1","dist spam=0","punto de corte"),
         lty=1,col=c("red","darkblue","green"))
  #clase predicha con el punto de corte
  clase_predicha<-as.integer(probas>cutoff)
  #metricas
  cm<-table(clase_predicha,dataset$spam)
  cm<-confusionMatrix(cm,positive = "1")
  print(cm$overall[1])
  print(cm$byClass[1])
  print(cm$byClass[2])
  return(list(clase_predicha=clase_predicha,
              confussion_matrix=cm,
              cutoff=cutoff))
}


#Se carga libreria para carga de datos desde github
library(RCurl)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/Ricardo27cruz27/spamFilter/master/datos_spam.csv"
url_csv<-getURL(url)
datos<-read.csv(text=url_csv,header = F)
#Se eliminan los índices de las variables
datos<-datos[-c(4602,4603),]

#Nombres de las variables:
names(datos)<-c('make','address','all','3d','our','over','remove','internet','order','mail','receive','will','people','report','addresses','free','business','email','you','credit','your','font','0','money','hp','hpl','george','650','lab','labs','telnet','857','data','415','85','technology','1999','parts','pm','direct','cs','meeting','original','project','re','edu','table',
                'conference',
                'char_freq_;',
                'char_freq_(',
                'char_freq_[',
                'char_freq_!',
                'char_freq_$',
                'char_freq_#',
                'capital_run_length_average',
                'capital_run_length_longest',
                'capital_run_length_total',
                'spam')

names(datos)
#Variables que son frecuencias de palabras:
palabras<-datos[,1:54]

#Variable objetivo
spam<-as.factor(datos[,"spam"])

#Palabras más frecuentes por categoría
prueba<-cbind(palabras,spam)
spam1<-sqldf("SELECT * from datos WHERE spam=1")
dim(spam1)
spam0<-sqldf("SELECT * from datos WHERE spam=0")
dim(spam0)
#palabra más frecuente por categoría
vec1<-apply(spam1[,1:48], 1, which.max)
sort(summary(as.factor(names(spam1)[vec1])))
vec0<-apply(spam0[,1:48], 1, which.max)
sort(summary(as.factor(names(spam0)[vec0])))

#Conjunto de entrenamiento y prueba
set.seed(10)
train<-sample(1:length(spam),size = 3680)
test=-train
datos_train<-datos[train,]
datos_test<- datos[test,]

#Correlación de las variables
x11()
corrplot(cor(datos), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")
x11()
corrplot(cor(spam1), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")
x11()
corrplot(cor(spam0), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")


#Regresion logistica
logistica<-glm(as.factor(spam)~.,
               data=datos_train,
               family = binomial("logit"),
               maxit = 500)
logistica
#Probabilidades en 0 o 1.
summary(as.factor(logistica$fitted.values))

#FULL CUTOFF FIJO A .5
log_train_5<-densidades(logistica,
                       datos_train,
                       cutoff = .5)
log_test_5<-densidades(logistica,
                       datos_test,
                       cutoff = .5)

#ZERO FALSE POSITIVES:
probas<-predict(modelo,dataset,type="response")
prob_nsr<-probas[which(dataset$spam==0)]
log_train_max<-densidades(logistica,
                        datos_train,
                        cutoff = max(prob_nsr))
log_test_max<-densidades(logistica,
                       datos_test,
                       cutoff = max(prob_nsr))


#FULL CUTOFF LIBRE
log_train_libre<-densidades(logistica,
                       datos_train,
                       cutoff = 0)
log_test_libre<-densidades(logistica,
                       datos_test,
                       cutoff = 0)


library(MASS)
library(tidyverse)
seleccion<-logistica%>%stepAIC(trace = FALSE,direction = "both")
#seleccionfwd<-logistica%>%stepAIC(trace = FALSE,direction = "forward")
seleccion$aic

names(seleccion$coefficients)
aux<-seleccion$formula

log_sw<-glm(aux,
            data=datos_train,
            family = binomial("logit"),
            maxit = 500)

#SELECCION CUTOFF LIBRE TRAIN
prueba_log_sw<-densidades(log_sw,
                          datos_train,
                          cutoff = 0)
#SELECCION CUTOFF LIBRE TEST
prueba_log_sw<-densidades(log_sw,
                          datos_test,
                          cutoff = 0)

checar<-function(x){return(sum(diff(x)))}
columnas1<-apply(spam1,2,checar)
columnas0<-apply(spam0,2,checar)
sort(columnas1)
sort(columnas0)

which(abs(columnas1)>.1)
which((columnas0)>.1)

#LA SEPARACION QUASI PERFECTA SE GENERA POR 
#which(columnas1==0)

#REALICEMOS UNA REGRESION SIN ESA VARIABLES:
qua<-unique(c(which(abs(columnas1)>1),
              which((columnas0)>1),58))
qua

log_sin_quasi<-glm(as.factor(spam)~.,
               data=datos_train[,qua],
               family = binomial("logit"),
               maxit = 500)

log_quasi_libre<-densidades(log_sin_quasi,
                            datos_train,
                            0)


sort((log_sw$coefficients))

#No se corrige la separación completa

# LOS MODELOS FULL Y STEPWISE TIENEN
# SEPARACION COMPLETA. SE INTENTARA
# CV, PLS, REGRESION BAYESIANA, SVM,NAIVE



#lasso:
library(glmnet)
# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(as.matrix(datos_train[,-58]),
                      as.matrix(as.factor(datos_train[,58])),
                      alpha = 1, family = "binomial")
x11();plot(cv.lasso)
# Fit the final model on the training data
model <- glmnet(as.matrix(datos_train[,-58]),
                as.matrix(datos_train[,58]),
                alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
sum(coef(model)[,1]!=0)
# Make predictions on the test data
x.test <- model.matrix(spam~., datos_test)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- datos_test[,58]
mean(predicted.classes == observed.classes)

cm<-table(predicted.classes,observed.classes)
cm<-confusionMatrix(cm,positive = "1")
print(cm$overall[1])
print(cm$byClass[1])
print(cm$byClass[2])


#ridge:
# Find the best lambda using cross-validation
cv.ridge <- cv.glmnet(as.matrix(datos_train[,-58]),
                      as.matrix(datos_train[,58]),
                      alpha = 0, family = "binomial")
x11();plot(cv.ridge)
# Fit the final model on the training data
model <- glmnet(as.matrix(datos_train[,-58]),
                as.matrix(datos_train[,58]),
                alpha = 0, family = "binomial",
                lambda = cv.ridge$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(spam~., datos_test)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- datos_test[,58]
mean(predicted.classes == observed.classes)
cm<-table(predicted.classes,observed.classes)
cm<-confusionMatrix(cm,positive = "1")
print(cm$overall[1])
print(cm$byClass[1])
print(cm$byClass[2])


#elastic net:
# Find the best lambda using cross-validation
cv.enet <- cv.glmnet(as.matrix(datos_train[,-58]),
                      as.matrix(datos_train[,58]),
                      alpha = 0.45, family = "binomial")
x11();plot(cv.enet)
# Fit the final model on the training data
model <- glmnet(as.matrix(datos_train[,-58]),
                as.matrix(datos_train[,58]),
                alpha = 0, family = "binomial",
                lambda = cv.enet$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(spam~., datos_test)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- datos_test[,58]
mean(predicted.classes == observed.classes)




#SVM
datos_train2<-datos_train
datos_train2$spam<-as.factor(datos_train$spam)
svm <- train(
  spam ~., data = datos_train2, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)
# Make predictions on the test data
predicted.classes <- svm %>% predict(datos_test)
mean(predicted.classes == observed.classes)
cm<-table(predicted.classes,observed.classes)
cm<-confusionMatrix(cm,positive = "1")
print(cm$overall[1])
print(cm$byClass[1])
print(cm$byClass[2])


svm2 <- train(
  spam ~., data = datos_train2, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)
# Make predictions on the test data
predicted.classes <- svm2 %>% predict(datos_test)
mean(predicted.classes == observed.classes)


svm3 <- train(
  spam ~., data = datos_train2, method = "svmPoly",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)
# Make predictions on the test data
predicted.classes <- svm3 %>% predict(datos_test)
mean(predicted.classes == observed.classes)



library(klaR)
#NAIVE BAYES
NB <- train(spam ~., data = datos_train2, method = "nb", 
               trControl = trainControl("cv", number = 10))
# Make predictions
predicted.classes <- NB %>% predict(datos_test)
# Model n accuracy
mean(predicted.classes == observed.classes)



#LOGISTIC BAYESIANA
library(arm)
mod_bayes<-bayesglm(formula = spam~.,
                    family=binomial,
                    data=datos_train)

result_bayes_train<-densidades(mod_bayes,
                               datos_train,
                               cutoff = 0)
result_bayes_test<-densidades(mod_bayes,
                               datos_test,
                               cutoff = result_bayes_train$cutoff)
