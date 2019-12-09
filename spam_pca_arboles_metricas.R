library("wordcloud")
library(dplyr)
library(ggplot2)


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
length()

datos.pca <- prcomp(datos[,-ncol(datos)], 
                    center = TRUE, scale = TRUE)
pcaCharts(datos.pca)