# Librerías y datos -------------------------------------------------------
library(tidyverse)
datos <- read.csv("../0_datos/indicadores_finales_top_10_2010_2020.csv")

# Identificar los cantantes con más canciones -----------------------------
# 1. Se borran de la base de datos las canciones que se han repetido en el top.
datos <- datos %>% 
  distinct(titulo, .keep_all = T)

# 2. Se separa la columna de artistas y se pasa a formato largo, donde hay una 
# observación por cada artista que ha participado en cada canción, en lugar de 
# una única observación por canción.
separados <- datos %>% 
  separate(cantantes,
           into = c("artista", "artista2", "artista3", "artista4", "artista5"),
           sep = "\\s*(/|,|FEAT\\.)\\s*") %>% 
  pivot_longer(
    cols = starts_with("artista"),
    names_to = "tipo_artista",    
    values_to = "nombre_artista",
    values_drop_na = TRUE) 

# 3. Se elimina de la base a aquellos cantantes que nunca han tenido más de una
# canción en el top.
repetidos <- separados %>% 
  group_by(nombre_artista) %>% 
  summarise(n_canciones = n()) %>%
  filter(n_canciones >=2)

# 4. Se extrae un vector con el nombre de los cantantes repetidos.
cantantes_repetidos <- repetidos %>% 
  pull(nombre_artista)

# 5. Se reduce el df dejando únicamente los cantantes de interés.
datos_cantantes_repetidos <- separados %>% 
  left_join(repetidos, by = "nombre_artista") %>% 
  filter(nombre_artista %in% cantantes_repetidos)


# Generar las variables para el gráfico -----------------------------------
# 1. Variable con el género musical predominante.
generos_cantantes_repetidos <- datos_cantantes_repetidos %>%
  group_by(nombre_artista, genero_agrupado) %>%
  summarise(n_veces_genero = n()) %>%
  arrange(-n_veces_genero) %>%
  distinct(nombre_artista, .keep_all = T) %>%
  select(nombre_artista, genero = genero_agrupado)


# 2. Se crea la variable con la proporción de indicadores por canción, agrupando
# la base datos a nivel cantante (y manteniendo las variables de interés).
proporcion_indicadores <- datos_cantantes_repetidos %>% 
  group_by(nombre_artista) %>% 
  summarise(suma_indicadores = sum(indicadores_totales,
                                   na.rm = T),
            n_canciones = first(n_canciones),
            genero_del_cantante = first(genero_del_cantante)) %>% 
  mutate(proporcion_indicadores = (suma_indicadores / n_canciones) %>% 
           round(1))


# 3. Se unen las dos bases de datos.
plot_burbujas <- left_join(proporcion_indicadores,
                           generos_cantantes_repetidos,
                           by = "nombre_artista") %>% 
  mutate(nombre_artista = str_to_title(nombre_artista))



# Guardar en Excel --------------------------------------------------------
# En este documento se añadirán a mano los enlaces a las fotos de cada uno
# de los cantantes. Posteriormente se subira a Flourish para crear el gráfico.

writexl::write_xlsx(plot_burbujas, "0_datos/g_burbujas_cantantes.xlsx")


