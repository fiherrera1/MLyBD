library(readr)
library(tidyverse)
library(ggplot2)
credit_data <- read_csv("credit-data.csv")
credit_data$X1 <- NULL

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

## Tarea 1
## A.

## Default

estadisticos_duration <- summarise(credit_data, min = min(duration), max = max(duration), media = mean(duration), sd = sd(duration))
estadisticos_duration

plot(density(duration), font = 4, main = "Función de densidad", sub = "Variable duration", xlab = "", ylab = "", 
     cex.main = 2.5, cex.sub = 2, col = "darkred", col.main = "azure4", col.sub = "azure3", fg = "orange", col.axis = "maroon2")

estadisticos_amount <- summarise(credit_data, min = min(amount), max = max(amount), media = mean(amount), sd = sd(amount))
estadisticos_amount

plot(density(amount), font = 4, main = "Función de densidad", sub = "Variable amount", xlab = "", ylab = "", 
     cex.main = 2.5, cex.sub = 2, col = "darkred", col.main = "azure4", col.sub = "azure3", fg = "orange", col.axis = "maroon3")

estadisticos_age <- summarise(credit_data, min = min(age), max = max(age), media = mean(age), sd = sd(age))
estadisticos_age

plot(density(age), font = 4, main = "Función de densidad", sub = "Variable age", xlab = "", ylab = "", 
     cex.main = 2.5, cex.sub = 2, col = "darkred", col.main = "azure4", col.sub = "azure3", fg = "orange", col.axis = "maroon")

## B

tabla_history <- prop.table(table(history))
tabla_history
barplot(tabla_history,las=1, col = c("cyan","aquamarine","cadetblue"),legend.text=c("good","poor","terrible"),xlim=c(0,4),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable history", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_purpose <- prop.table(table(purpose))
tabla_purpose
barplot(tabla_purpose,las=1, col = c("darkorange","darkorange1","darkorange2","darkorange3","darkorange4"),legend.text=c("biz","edu","goods/repair","newcar","usedcar"),xlim=c(0,6),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable purpose", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_foreign <- prop.table(table(foreign))
tabla_foreign
barplot(tabla_foreign,las=1, col = c("aliceblue","cornflowerblue"),legend.text=c("foreign","german"),xlim=c(0,3),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable foreign", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_rent <- prop.table(table(rent))
tabla_rent
barplot(tabla_rent,las=1, col = c("palevioletred1","palevioletred"),legend.text=c("FALSE","TRUE"),xlim=c(0,3),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable rent", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_savings <- prop.table(table(savings))
tabla_savings
barplot(tabla_savings,las=1, col = c("peachpuff","peachpuff1","peachpuff2","peachpuff3","peachpuff4"),legend.text=c("< 100 DM",">=1000 DM","100 to 500 DM","500 to 1000 DM","unknown or no"),xlim=c(0,6),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable savings", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_Default <- prop.table(table(Default))
tabla_Default
barplot(tabla_Default,las=1, col = c("deeppink3","deeppink4"),legend.text=c("0","1"),xlim=c(0,3),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable Default", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_residence <- prop.table(table(residence))
tabla_residence
barplot(tabla_residence,las=1, col = c("darkslategray1","darkslategray2","darkslategray3","darkslategray4"),legend.text=c("1","2","3","4"),xlim=c(0,5),ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable residence", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

tabla_installment <- prop.table(table(installment))
tabla_installment
barplot(tabla_installment,las=1, col = c("darkseagreen1","darkseagreen2","darkseagreen3","darkseagreen4"),legend.text=c("1","2","3","4"),xlim=c(0,5), ylim = c(0,1),
        main = "Histograma/Grafico de barras", sub = "Variable installment", cex.main = 2.5, cex.sub = 2,
        ylab ="Frecuencias Relativas",font.axis=4, col.main = "darkslategray", col.sub = "darkslategray4", fg = "darkred", col.axis = "goldenrod")

## C y D
Variable_Default <- factor(Default)
# Variables numericas
ggplot(data=credit_data) + geom_density(aes(x=amount, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Densidad según variable Default", y = "Densidad", subtitle = "Variable amount") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_density(aes(x=age, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Densidad según variable Default", y = "Densidad", subtitle = "Variable age") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_density(aes(x=duration, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Densidad según variable Default", y = "Densidad", subtitle = "Variable duration") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

#Variables nominales
ggplot(data=credit_data) + geom_bar(aes(x=history, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable history") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=purpose, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable purpose") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=foreign, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable foreign") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=rent, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable rent") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=savings, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable savings") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=residence, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable residence") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_bar(aes(x=installment, fill = Variable_Default), alpha= 0.5) + 
        labs(title = "Frecuencia según variable Default", y = "Frecuencia", subtitle = "Variable installment") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

## E
ggplot(data=credit_data) + geom_point(aes(x=amount,y=age, colour = Variable_Default), alpha= 0.5) + 
        labs(title = "Scatterplot", subtitle = "Variables amount y age") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_point(aes(x=duration,y=age, colour = Variable_Default), alpha= 0.5) + 
        labs(title = "Scatterplot", subtitle = "Variables duration y age") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))

ggplot(data=credit_data) + geom_point(aes(x=amount,y=duration, colour = Variable_Default), alpha= 0.5) + 
        labs(title = "Scatterplot", subtitle = "Variables amount y duration") +
        theme(plot.title = element_text(hjust = 0.5,face = "bold",colour = "firebrick4"), plot.subtitle = element_text(hjust = 0.5,face = "bold",colour = "firebrick3"))
