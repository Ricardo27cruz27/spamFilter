library("wordcloud")
library(dplyr)
library(ggplot2)
library(caret)
library(Metrics)



sumrows(palabras)
####################
#funcion para graficas sobre pca
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,1))
  plot(x.pvar,xlab="Componente principal", ylab="Proporción de varianza explicada", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Componente principal", ylab="Proporcion de varianza explicada acumulada", ylim=c(0,1), type='b')
  par(mfrow=c(1,1))
}

#################

#suma agregada por clase en las variables de frecuencia
range0100 <- function(x){100*(x-min(x))/(max(x)-min(x))}

resumen_freq_clase <- prueba %>% group_by(spam) %>% summarise_all(sum)

resumen_freq_0 <- as.data.frame(resumen_freq_clase[1,][-1])
resumen_freq_1 <- as.data.frame(resumen_freq_clase[2,][-1])

resumen_freq_0 <- range0100(as.vector(resumen_freq_0))
resumen_freq_1 <- range0100(as.vector(resumen_freq_1))


#wordclouds de palabras más frecuenctes por clase
wordcloud(words = vars_freq, freq = as.data.frame(resumen_freq_0), random.order=FALSE, 
          scale = c(2,2),rot.per=0.45,colors=brewer.pal(8, "Dark2"), max.words=20)

wordcloud(words = vars_freq, freq = as.data.frame(resumen_freq_1), random.order=FALSE, 
          scale = c(2,2),rot.per=0.35,colors=brewer.pal(8, "Dark2"), max.words=20)

###### 
#pca

datos.pca <- prcomp(datos[,-ncol(datos)], 
                    center = TRUE, scale = TRUE)
pcaCharts(datos.pca)

########
#arboles de decision
library(rpart)
library(rpart.plot)


control <- rpart.control(maxdepth = 5, cp = .005)
#entre 0 y 1
#clasificar 0 como 1
L <- matrix(c(0,2,1,0), byrow=TRUE, nrow=2)

arbol_fit <- rpart(spam~., data = datos_train, method = 'class', 
                   parms = list(loss= L), control = control)

#cp es lo que cuesta agregar una nueva variable
printcp(arbol_fit)
plotcp(arbol_fit)

arbol_predict <-predict(arbol_fit, datos_test, type = 'class')
confusionMatrix(arbol_predict, as.factor(datos_test$spam), positive='1')
rpart.plot(arbol_fit, extra = 106)
#RF
