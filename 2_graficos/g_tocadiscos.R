# Librería y datos --------------------------------------------------------
library(tidyverse)

datos_originales <- read.csv("../0_bases_datos/indicadores_finales_top_10_2010_2020.csv")


# Medias indicadores según el género musical ------------------------------

datos <- datos_originales %>%
  group_by(genero_agrupado) %>%
  summarise(media_feminismo = mean(indicadores_feminista, na.rm = TRUE),
            media_machismo = mean(indicadores_totales, na.rm = TRUE))



# Tocadiscos 1: feminismo -------------------------------------------------

# Marcamos un tamaño para el círculo central, 
# de cara a poder integrar el gráfico visualmente en el tocadiscos
base_interno <- 0.39

datos_grafico <- datos %>%
  mutate(valor_ajustado = media_feminismo + base_interno) 

discofem <- datos_grafico %>%
  ggplot(aes(x = genero_agrupado,
             fill = genero_agrupado)) +
  
  geom_bar(aes(y = valor_ajustado),
           stat = "identity",
           position = "dodge") +
  
  geom_hline(yintercept = 0.39) +
  
  coord_polar() +
  
  scale_fill_manual(values = c(
    "Electronica" = "#00FFFF",
    "Pop" = "#9820c9",
    "Hiphop" = "#7200A1",
    "regueton" = "#B500FF"
  ))  +
  
  theme_void() +
  
  ylim(0, max(datos$media_feminismo) + base_interno)

# ggsave("discofem.png",
#        plot = discofem,
#        width = 6,
#        height = 6,
#        dpi = 300)


# Tocadiscos 2: machismo --------------------------------------------------

base_interno2 <- 1.5

datos_grafico2 <- datos %>%
  mutate(valor_ajustado2 = media_machismo + base_interno2) 

discomac <- datos_grafico2 %>%
  ggplot(aes(x = genero_agrupado,
             fill = genero_agrupado)) +
  
  geom_bar(aes(y = valor_ajustado2),
           stat = "identity",
           position = "dodge") +
  
  geom_hline(yintercept = 1.5) +
  
  coord_polar() +
  scale_fill_manual(values = c(
    "Electronica" = "#00FFFF",
    "Pop" = "#A0A0A0",
    "Hiphop" = "#4D4D4D",
    "regueton" = "#1F1F1F"
  )) +
  
  theme_void() +

  ylim(0, max(datos$media_machismo) + base_interno2)

# ggsave("discomac.png",
#        plot = discomac,
#        width = 6,
#        height = 6,
#        dpi = 300)
