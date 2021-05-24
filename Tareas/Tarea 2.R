library(readr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(fpc)
credit_data <- read.csv("C:/Users/fiher/Desktop/UC/Big Data y Machine Learning/Tareas/Tarea 2/credit-data.csv")
credit_data$X <- NULL

solo_num <- credit_data
solo_num$history <- NULL
solo_num$purpose <- NULL
solo_num$savings <- NULL
solo_num$rent <- NULL
solo_num$foreign <- NULL
solo_num$residence <- NULL
solo_num$installment <- NULL
solo_num$Default <- NULL

# Variables numericas

duration <- solo_num$duration
amount <- solo_num$amount
age <- solo_num$age

# Variables
residence <- credit_data$residence
installment <- credit_data$installment
Default <- credit_data$Default
history <- credit_data$history
purpose <- credit_data$purpose
foreign <- credit_data$foreign
rent <- credit_data$rent
savings <- credit_data$savings

# Tarea 2

# Primero normalizo las variables
normalizadas <- function(x) { (x -min(x))/(max(x)-min(x))  }
data_norm <- as.data.frame(lapply(solo_num[,1:3], normalizadas))
dis = dist(data_norm)

# Creo el cluster de dos centros
cluster <- kmeans(data_norm, 2, nstart = 25)

# Calculo el coeficientes de Silhouette y el Calinski-Harabaz score
silhouette(cluster$cluster, dis)[,3]
sil <- silhouette(cluster$cluster, dis)
mean(sil[,3])
calinhara(data_norm,cluster$cluster,2)

# Ahora procedo a con la pregunta 4.

datos <- c()
varios_sil <- c()
varios_cal <- c()
for (i in 2:20) {
  k <- kmeans(data_norm[,1:3], i, nstart = 25)
  datos <- append(datos, i)
  varios_sil <- append(varios_sil, mean(silhouette(k$cluster,dis)[,3]))
  varios_cal <- append(varios_cal, calinhara(data_norm,k$cluster,i))
}


plot_data <- as.data.frame(cbind(varios_datos,varios_sil,varios_cal))

ggplot(plot_data,aes(x = varios_datos)) + geom_line(aes(y = varios_cal, colour = "Calinski-Harabaz Score"))+
  geom_line(aes(y = varios_sil*1300, colour = "Silhouette Coefficient")) + 
  scale_y_continuous(sec.axis = sec_axis(~./1300,name = "varios_sil"))+
  scale_x_continuous(breaks = varios_datos) + theme_classic() + labs(colour = "Metrics") + ggtitle("K-means Metrics")


