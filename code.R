#importamos los datos a Rstudio
tormentas <- read.csv("~/Downloads/tormentas EEUU csv.csv")

#cargamos la libreria tidyverse
library(tidyverse)

#comenzamos filtrando los datos para mejorar la sensibilidad del estudio 
#aunque esto no es lo mas recomendable como nuestro objetivo era revisar 
#la cantidad de personas afectadas tiene sentido.

filtro <- filter(tormentas, INJURIES == 0)
filtro1 <- filter(tormentas, INJURIES > 0)

#continuamos agrupando por evento 
agrupar <- filtro1 %>%
        group_by(EVTYPE) 

#la primer visual no la cargamos a el informe final solo el resultado 
ggplot(agrupar) +
        stat_summary(
                mapping = aes(EVTYPE, INJURIES),
                fun.min = min,
                fun.max = max,
                fun = median
        )

#aumentamos los heridos a mas de 50 para establecer un numero de heridos 
#que nos permitan llegar a los eventos con mayores repercusiones en la salud
#de las personas 
filtro2 <- filter(agrupar, INJURIES >= 50)
seleccion <- select(filtro2, INJURIES, EVTYPE, STATE)
seleccion1 <- complete.cases(seleccion)
seleccion2 <- seleccion[seleccion1, ]

#obtenemos una visual del filtrado 
ggplot(seleccion2) +
        stat_summary(
                mapping = aes(EVTYPE, INJURIES, col = EVTYPE),
                fun.min = min,
                fun.max = max,
                fun = median
        )

#indentificados los eventos mas debastadores los individualizamos para 
#poder visualizarlos y decidir cual es el mas debastador 

ice <- filter(seleccion2, EVTYPE == "ICE STORM")
Tornado <- filter(seleccion2, EVTYPE == "TORNADO")
flood <- filter(seleccion2, EVTYPE == "FLOOD")
Hurricane <- filter(seleccion2, EVTYPE == "HURRICANE/TYPHOON")

# poniendolos en una celocia
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

#los programamos para luego ponerlos en unas celosias 
hist1 <- hist(ice$INJURIES, main = "ICE STORM", xlab = "numero de afectados",
              ylab = "cantidad de eventos")

hist2 <- hist(Tornado$INJURIES, main = "TORNADO", xlab = "numero de afectados",
              ylab = "cantidad de eventos")

hist3 <- hist(flood$INJURIES, main = "FLOOD", xlab = "numero de afectados",
              ylab = "cantidad de eventos")

hist4 <- hist(Hurricane$INJURIES, main = "HURRICANE TYPHOON", xlab = "numero de afectados",
              ylab = "cantidad de eventos")

ggplot(filtro2) +
        geom_bar(mapping = aes(STATE, col = STATE))




