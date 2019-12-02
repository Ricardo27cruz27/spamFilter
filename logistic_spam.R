### FILTRO DE SPAM ###

library(LaplacesDemon)

#Se carga libreria para carga de datos desde github
library(RCurl)

#Carga de la base de datos
url<-"https://raw.githubusercontent.com/Ricardo27cruz27/spamFilter/master/datos_spam.csv"
url_csv<-getURL(url)
datos<-read.csv(text=url_csv,header = F)
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
palabras<-datos[,1:48]

#Variable objetivo
spam<-datos[,"spam"]

prueba<-cbind(palabras,spam)

spam1<-sqldf("SELECT * from prueba WHERE spam=1")
dim(spam1)
spam0<-sqldf("SELECT * from prueba WHERE spam=0")
dim(spam0)

wordcloud(spam1[,1:48])

summary(as.factor(apply(spam1, 1, which.max)))

vec1<-apply(spam1[,1:48], 1, which.max)
sort(summary(as.factor(names(spam1)[vec1])))

vec0<-apply(spam0[,1:48], 1, which.max)
sort(summary(as.factor(names(spam0)[vec0])))


library(corrplot)
train<-sample(1:length(spam),size = 3680)
test=-train

datos_train<-datos[train,]
datos_test<- datos[test,]

x11()
corrplot(cor(datos), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

#Regresion logistica
logistica<-glm(spam~.,
               data=datos_train,
               family = binomial("logit"),
               maxit = 500)
logistica

#Probabilidades en 0 o 1.
summary(as.factor(logistica$fitted.values))

length(which(datos_train$spam==1))
length(which(datos_train$spam==0))

train_spam_real<-datos[which(datos_train$spam==1),58]
train_nospam_real<-datos[which(datos_train$spam==0),58]


probas<-invlogit(predict(logistica,datos_train))
prob_sr<-probas[which(datos_train$spam==1)]
prob_nsr<-probas[which(datos_train$spam==0)]

den_prob_sr<-density(prob_sr,from = 0,to=1)
den_prob_nsr<-density(prob_nsr,from=0,to=1)

plot(den_prob_sr)
plot(den_prob_nsr)

x11()
plot(den_prob_sr,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="red")
par(new=T)
plot(den_prob_nsr,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="darkblue")
abline(v=.5,col="green")
legend(x="topright",legend=c("dist spam=1","dist spam=0","punto de corte"),
       lty=1,col=c("red","darkblue","green"))

cutoff<-.5
clase_predicha<-as.integer(probas>cutoff)

library(caret)
cm.train<-table(clase_predicha,datos_train$spam)
cm.train<-confusionMatrix(cm.train,positive = "1")
cm.train$overall[1]
cm.train$byClass[1]
cm.train$byClass[2]


probas_test<-invlogit(predict(logistica,datos_test))



seleccion<-step(mod, scope=list(lower=mod, upper=modelo2), direction="forward")

library(MASS)
library(tidyverse)
seleccion<-logistica%>%stepAIC(trace = FALSE)

names(seleccion$coefficients)
dim(seleccion$model)
aux<-seleccion$formula

log_sw<-glm(aux,
            data=datos_train,
            family = binomial("logit"),
            maxit = 500)

probs_sw<-predict(log_sw,datos_train,type="response")
summary(as.factor(probs_sw))

prob_sr_sw<-probs_sw[which(datos_train$spam==1)]
den_prob_sr_sw<-density(prob_sr_sw,from = 0,to=1)
prob_nsr_sw<-probs_sw[which(datos_train$spam==0)]
den_prob_nsr_sw<-density(prob_nsr_sw,from = 0,to=1)


plot(den_prob_sr_sw)
plot(den_prob_nsr)

x11()
plot(den_prob_sr_sw,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="red")
par(new=T)
plot(den_prob_nsr_sw,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="darkblue")
abline(v=.36,col="green")
legend(x="topright",legend=c("dist spam=1","dist spam=0","punto de corte"),
       lty=1,col=c("red","darkblue","green"))

cutoff<-.36
clase_predicha_sw<-as.integer(probs_sw>cutoff)

cm.train<-table(clase_predicha_sw,datos_train$spam)
cm.train<-confusionMatrix(cm.train,positive = "1")
cm.train$overall[1]
cm.train$byClass[1]
cm.train$byClass[2]






#test
probs_sw_ts<-predict(log_sw,datos_test,type="response")
#summary(as.factor(probs_sw_ts))

prob_sr_sw_ts<-probs_sw_ts[which(datos_test$spam==1)]
den_prob_sr_sw_ts<-density(prob_sr_sw_ts,from = 0,to=1)
prob_nsr_sw_ts<-probs_sw_ts[which(datos_test$spam==0)]
den_prob_nsr_sw_ts<-density(prob_nsr_sw_ts,from = 0,to=1)

x11()
plot(den_prob_sr_sw_ts,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="red")
par(new=T)
plot(den_prob_nsr_sw_ts,xlim=c(0,1),ylim=c(0,9),main="",xlab="p()",ylab="",col="darkblue")
abline(v=.36,col="green")
legend(x="topright",legend=c("dist spam=1","dist spam=0","punto de corte"),
       lty=1,col=c("red","darkblue","green"))

clase_predicha_sw_ts<-as.integer(probs_sw_ts>cutoff)

cm.test<-table(clase_predicha_sw_ts,datos_test$spam)
cm.test<-confusionMatrix(cm.test,positive = "1")
cm.test$overall[1]
cm.test$byClass[1]
cm.train$byClass[2]
