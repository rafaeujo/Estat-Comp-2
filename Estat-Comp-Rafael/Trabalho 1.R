#1
##Rodando Bibliotecas
library(dplyr)
library(esquisse)
library(ggplot2)
library(tidyr)

#https://www.kaggle.com/datasets/abdulszz/spotify-most-streamed-songs/data

##Rodando dataset e panorama inicial
bas <- read.csv("Spotify Most Streamed Songs.csv")
head(bas)
tail(bas)
is.na(bas)
glimpse(bas)

#2
##Verificando quantidade de dados faltantes por coluna
sapply(bas, function(x) sum(is.na(x)))

##Plotando Histograma de cada coluna
lapply(names(bas), function(col) {
  if (is.numeric(bas[[col]])) {
    hist(bas[[col]], main = paste("Histograma de", col), xlab = col)
  }
})

#Selecionando colunas de interesse
baselec <- bas %>% 
  select(track_name:bpm) %>% 
  select(-in_shazam_charts)

#Limpando as colunas 
baselec$streams <- as.integer(baselec$streams)
baselec$in_deezer_playlists <- as.integer(baselec$in_deezer_playlists)
baselec <- na.omit(baselec)


baselec <- baselec %>%
  mutate(playlists_total = in_spotify_playlists + in_apple_playlists + in_deezer_playlists) %>% 
  select(-in_spotify_playlists,-in_apple_playlists,-in_deezer_playlists)

basboxplot <- baselec %>% 
  select(artist_count:playlists_total)

#Plotando boxplots
par(mfrow = c(1,1))

for (coluna in colnames(basboxplot)) {
  boxplot(basboxplot[[coluna]], 
          main = paste("Boxplot -", coluna), 
          col = "skyblue", 
          ylab = "Valores",
          outline = TRUE)  # Mostra outliers
}
