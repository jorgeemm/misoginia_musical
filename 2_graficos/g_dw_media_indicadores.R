# Cargar librerías --------------------------------------------------------
library(tidyverse)
library(DatawRappr)


# Apertura y transformación de los datos ----------------------------------
datos <- read.csv("../0_bases_datos/indicadores_finales_top_10_2010_2020.csv")

# Transformar en formato largo
datos_indicadores <- datos %>% 
  mutate(i_a7 = i_a7 + i_c2,
         i_a5 = i_a5 + i_c3) %>% 
  
  pivot_longer(
    cols = c(starts_with("i_a"), "i_c1"),
    names_to = "tipo_indicador",
    values_to = "n_apariciones"
  ) %>% 
  
  # Se les cambian los nombres a los indicadores
  mutate(tipo_indicador = fct_recode(tipo_indicador,
                                     "Amor tóxico" = "i_a5",
                                     "Cosificación" = "i_a1",
                                     "Control" = "i_a2",
                                     "No es Sí" = "i_a3",
                                     "Competencia" = "i_a7",
                                     "Roles tradicionales" = "i_a4",
                                     "Lenguaje explícito" = "i_a6",
                                     "Valicación masculina" = "i_c1"))


# Calcular la media y los errores -----------------------------------------
plot_media_i <- datos_indicadores %>%
  group_by(tipo_indicador) %>% 
  summarise( 
    n = n(),
    media = mean(n_apariciones) %>% 
      round(2),
    sd = sd(n_apariciones)
  ) %>%
  
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + .5, n-1),
    l_inferior = (media - ic) %>% 
      round(2),
    l_superior = (media + ic) %>% 
      round(2),
    # Se crea también una variable para ordenarlos de mayor a menor
    posicion_i = rank(-media, ties.method = "first")) %>%
  
  select(media, l_inferior, l_superior, tipo_indicador, posicion_i)



# Crear la paleta de colores ----------------------------------------------
colores <- c("Cosificación" = "#e3e3e3",
             "Control" = "#c674ff",
             "No es Sí" = "#e3e3e3",
             "Roles tradicionales" = "#e3e3e3",
             "Amor tóxico" = "#c674ff",
             "Lenguaje explícito" = "#c674ff",
             "Competencia" = "#c674ff",
             "Valicación masculina" = "#e3e3e3")


# Gráfico preeliminar con ggplot ------------------------------------------
ggplot(plot_media_i)  +
  geom_bar(aes(x = reorder(tipo_indicador, -media),
               y = media,
               fill = tipo_indicador),
           stat = "identity",
           color = "#ffffff",
           alpha = 0.5) +
  geom_pointrange(aes(x = tipo_indicador,
                      y = media,
                      ymin = l_inferior,
                      ymax = l_superior),
                  color = "#676767",
                  size = 0.2) +
  scale_fill_manual(values = colores) +
  theme_classic() +
  theme(legend.position = "none")


# Preparación gráfico DW --------------------------------------------------
# Generar un nuevo gráfico de puntos
# grafico <- dw_create_chart(type="d3-scatter-plot")

# Añadirle los datos
dw_data_to_chart(plot_media_i, grafico)


# Añadir el vector de colores al df
plot_media_i <- plot_media_i %>% 
  mutate(color = colores[tipo_indicador])

# Asignar los colores correspondientes con cada categoría
lista_colores <- NULL

lista_colores$map<- as_tibble_row(colores) %>%
  as.list()

dw_edit_chart(grafico,
              visualize = list("color-category" = lista_colores, 
                               "color-by-colum" = T))


# Gráfico -----------------------------------------------------------------

# Crear las columnas
dw_width <- 0.5

columnas_min <- plot_media_i %>%
  mutate(value = l_inferior, 
         opacity = 0.4) %>%
  mutate(inferior_izq = str_glue("{posicion_i - dw_width/2}, 0,"),
         superior_izq = str_glue("{posicion_i - dw_width/2}, {value},"),
         superior_dcha = str_glue("{posicion_i + dw_width/2}, {value},"),
         inferior_dcha = str_glue("{posicion_i + dw_width/2}, 0,"),
         estilo = str_glue("@color:{colores} @width:20 @opacity:{opacity}"),
         dw_range = str_glue("{inferior_izq}{superior_izq}{superior_dcha}{inferior_dcha}{estilo}")) %>%
  pull(dw_range) %>% 
  str_c(collapse = "\n")

columnas_max <- plot_media_i %>%
  mutate(value = l_superior,
         opacity = 0.4) %>%
  mutate(inferior_izq = str_glue("{posicion_i - dw_width/2}, 0,"),
         superior_izq = str_glue("{posicion_i - dw_width/2}, {value},"),
         superior_dcha = str_glue("{posicion_i + dw_width/2}, {value},"),
         inferior_dcha = str_glue("{posicion_i + dw_width/2}, 0,"),
         estilo = str_glue("@color:{colores} @width:20 @opacity:{opacity}"),
         dw_range = str_glue("{inferior_izq}{superior_izq}{superior_dcha}{inferior_dcha}{estilo}")) %>%
  pull(dw_range) %>% 
  str_c(collapse = "\n")

#Crear las líneas horizontales y verticales
lineas_media <- plot_media_i  %>%
  mutate(x = posicion_i,
         y = media) %>%
  transmute(tipo_indicador,
            colores,
            linea = str_glue("{x - 0.3}, {y}, {x + 0.3}, {y}") %>%
              as.character()) %>%
  mutate(line = str_glue("{linea} @color:{colores} @width:2 @opacity:0.95")) %>%
  pull(line) %>% 
  str_c(collapse = "\n")

lineas_vertical <- plot_media_i  %>%
  mutate(x = posicion_i + 0.45,
         y_min = l_inferior,
         y_max = l_superior) %>%
  transmute(tipo_indicador,
            colores,
            linea = str_glue("{x}, {y_min}, {x}, {y_max}") %>%
              as.character()) %>%
  mutate(line = str_glue("{linea} @color:{colores} @width:1.5 @opacity:0.65")) %>%
  pull(line) %>% 
  str_c(collapse = "\n")

# Unir todos los objetos y actualizar el gráfico
ranges <- str_c(columnas_min,"\n",
                columnas_max,"\n",
                lineas_media,"\n",
                lineas_vertical) 

dw_edit_chart(grafico, visualize = list("highlight-range" = ranges))

## Añadir etiquetas
# El nombre de los indicadores
nombre_indicador <- plot_media_i %>%
  transmute(
    x = posicion_i,
    y = 0.015,
    align = "mc", 
    bold = "TRUE",
    text = str_glue("{tipo_indicador}")) %>%
  mutate_all(as.character)

# Los números de las columnas (media)
cifra_media <- plot_media_i %>%
  transmute(
    x = posicion_i,
    y = media + 0.02,
    size = 12,
    color = "#e3e3e3",
    align = "bc",
    bg = "TRUE",
    text = media) %>%
  mutate_all(as.character)

# Los números de intervalos
cifra_inferior <- plot_media_i %>%
  transmute(
    x = posicion_i + 0.45,
    y = l_inferior,
    size = 10,
    color = colores,
    align = "tc", 
    text = l_inferior) %>%
  mutate_all(as.character)

cifra_superior <- plot_media_i %>%
  transmute(
    x = posicion_i + 0.45,
    y = l_superior,
    size = 10,
    color = colores,
    align = "bc",
    text = l_superior) %>%
  mutate_all(as.character)

textos <- bind_rows(
  nombre_indicador,
  cifra_media,
  cifra_superior,
  cifra_inferior) %>%
  purrr::transpose() %>%
  as.list()

dw_edit_chart(grafico,
              visualize = list("text-annotations" = textos))


## Información dentro de las burbujas
descripcion_indicadores <- data.frame(
  body = str_glue("Presencia media: {{{{media}}}} \n Limite máximo: {{{{l_superior}}}} \n Límite minimo: {{{{l_inferior}}}}"), 
  title = "{{tipo_indicador}}",
  sticky = FALSE,
  enabled = TRUE) %>% 
  as.list()

dw_edit_chart(grafico,
              visualize = list("tooltip" = descripcion_indicadores))












