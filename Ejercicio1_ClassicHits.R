library(dplyr)
library(ggplot2)

setwd("C:\\Users\\Rodrigo\\Documents\\Bootcamp AI\\Git\\Ejercicios_Curso_ML")

#Cargando mi data
classics <- read.csv("Datasets\\ClassicHit.csv", sep = ",",header=TRUE)
head(classics)

#viendo mis tipos de datos
str(classics)

#buscando nulos 
sapply(classics, function(x) sum(is.na(x)))

#cambiando los nulos por 0
classics$Danceability[is.na(classics$Danceability)] <- 0
classics$Instrumentalness[is.na(classics$Instrumentalness)] <- 0
sapply(classics, function(x) sum(is.na(x)))

#cambio de nombre de columnas a minúsculas
colnames(classics) <- tolower(colnames(classics))
head(classics)

#valores únicos por género
unique(classics$genre)

#valores a mayúsculas
classics$genre <- toupper(classics$genre)

#Valores unicos para alt. rock
classics$genre <- gsub('ALT.-ROCK|ALT.ROCK', 'ALT. ROCK', classics$genre)

#valores únicos por género
unique(classics$genre)

#agregar valores de las décadas
classics$decada = floor((classics$year / 10)) * 10 

#conteo de canciones por género
table(classics$genre)

#promedio de popularidad por artista
artistas_popularidad <- classics %>%
  group_by(artist) %>%
  summarise(promedio_popularidad = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(promedio_popularidad))

head(artistas_popularidad)

#promedio de popularidad por genero
genero_popularidad <- classics %>%
  group_by(genre) %>%
  summarise(promedio_popularidad = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(promedio_popularidad))

head(genero_popularidad)

#Artistas más populares por década

artistas_popularidad_deacada <- classics %>%
  group_by(artist,decada) %>%
  summarise(promedio_popularidad = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(promedio_popularidad))

head(artistas_popularidad_deacada)

#graficando los artistas más populares de x década
x_decada = 2000

top_5_artistas_decada <- artistas_popularidad_deacada %>%
  group_by(decada) %>%
  slice_head(n = 5)  

top_5_artistas_decada_grafica = subset(top_5_artistas_decada, decada == x_decada)

# Crear el gráfico de barras
ggplot(top_5_artistas_decada_grafica, aes(x = reorder(artist, -promedio_popularidad), y = promedio_popularidad, fill = decada)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Top 5 Artistas más Populares por Década",
       x = "Artistas",
       y = "Promedio de Popularidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # R

#Evolución de un artista
artista = 'Michael Jackson'

artista_decada = subset(classics, artist == artista)

popularidad_anio <- artista_decada %>%
  group_by(year) %>%
  summarise(promedio_popularidad = mean(popularity, na.rm = TRUE))

ggplot(popularidad_anio, aes(x = year, y = promedio_popularidad)) +
  geom_line(color = "blue") +  # Añadir la línea
  geom_point(color = "blue") +  # Opcional: añadir puntos en la línea
  labs(title = "Popularidad Promedio por Año",
       x = "Año",
       y = "Popularidad Promedio") +
  theme_minimal()  # Aplicar un tema minimalista


#Evolución de generos

generos_a_graficar = c('ALT. ROCK', 'POP', 'EDM', 'R&B', 'PUNK', 'TODAY','REGGAE')

generos <- classics %>%
    filter(genre %in% generos_a_graficar)

generos_decada <- generos %>%
  group_by(decada,genre) %>%
  summarise(promedio_popularidad = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(promedio_popularidad))

ggplot(generos_decada, aes(x = decada, y = promedio_popularidad, color = genre)) +
  geom_line(size = 1) +  # Añadir líneas
  geom_point(size = 2) +  # Opcional: añadir puntos en la línea
  labs(title = "Popularidad Promedio de Géneros Musicales por Año",
       x = "Año",
       y = "Popularidad Promedio") +
  theme_minimal() +
  theme(text = element_text(size = 12)) 






