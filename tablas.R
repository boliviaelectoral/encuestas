# sobre: tabla con enlace a fuentes primarias  y tabla a drive de acceso público
library(tidyverse)
library(googlesheets)
library(reactable)
library(magrittr)
library(extrafont)
library(htmltools)


# descarga del archivo
bd <- gs_title("Monitoreo de encuestas, drive privado")

# lista de hojas
# Monitoreo de encuestas, drive privado

# seleccion de hoja
df <- gs_read(ss = bd, ws = "Fuentes de las encuestas")

# sacar nota
nota <- df %>% 
  filter(str_detect(Encuestadora, "\\*")) %>% 
  pull(Encuestadora) 
  
# quitar filas y columnas
df %<>% 
  select(-X7, -`Código de encuesta`) %>% 
  filter(str_detect(Encuestadora, "Notas", negate = T)) %>% 
  filter(str_detect(Encuestadora, "\\*", negate = T))

# configurar tabla
reactable(df, filterable = TRUE, pagination = FALSE, columns = list(
  Encuestadora = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Medios de publicación` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de inicio de la encuesta` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de finalización de la encuesta` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Nº` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Enlaces a resultados y ficha técnica` = colDef(
    cell = function(value) {
      url <- paste0(value)
      tags$a(href = url, target = "_blank", paste0("enlace"))
    },
    style = list(
      fontFamily = "Roboto"
    )
   )
 )
) %>% 
  htmlwidgets::saveWidget(here::here("img", "tabla.html"))

#--------------------------------------------
# tablas sobre programas de gobierno

bd <- gs_title("Programas de Gobierno")


# seleccion de hoja
df <- gs_read(ss = bd, ws = "Comparación de programas en periódicos")

colnames(df) <- df %>% slice(1)
df %<>% slice(-1)

colnames(df)

reactable(df, filterable = TRUE, pagination = FALSE, wrap = T, columns = list(
  Tema = colDef(
    style = list(
      fontFamily = "Roboto",
      wrap = F
    )
  ),
  `Medio de publicación` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de publicación` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `N°` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  Fuente = colDef(
    cell = function(value) {
      url <- paste0(value)
      tags$a(href = url, target = "_blank", paste0("enlace"))
    },
    style = list(
      fontFamily = "Roboto"
    )
  )
)
) %>% 
  htmlwidgets::saveWidget(here::here("img", "planes_periodicos.html"))

# tabla de comparacion de programas òr instituciones
df <- gs_read(ss = bd, ws = "Comparación de programas por cuatro instituciones")

colnames(df) <- df %>% slice(1)
df %<>% slice(-1)

colnames(df)
reactable(df, filterable = TRUE, pagination = FALSE, wrap = T, columns = list(
  Nombre = colDef(
    style = list(
      fontFamily = "Roboto",
      wrap = F
    )
  ),
  Institución = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  Objetivo = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Nº` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  Temas = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de publicación` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  Fuente = colDef(
    cell = function(value) {
      url <- paste0(value)
      tags$a(href = url, target = "_blank", paste0("enlace"))
    },
    style = list(
      fontFamily = "Roboto"
    )
  )
)
) %>% 
  htmlwidgets::saveWidget(here::here("img", "planes_instituciones.html"))





#----------------------------------------
# procesamiento de tabla para público
encuestas <- gs_read(ss = bd, ws = "Base de datos") %>% 
  janitor::clean_names() %>% 
  select(-mes, -ano)

fichas <- gs_read(ss = bd, ws = "Fichas de las encuestas") %>% 
  janitor::clean_names()  %>% 
  select(-ano, -no, -metodo)

df_1 <- merge(encuestas, fichas, all.x = T)

#  cambio de nombres
df_1$partido_o_alianza %<>% gsub("CC", "Comunidad Ciudadana", .)
df_1$partido_o_alianza %<>% gsub("MTS", "Movimiento Tercer Sistema", .)
df_1$partido_o_alianza %<>% gsub("MNR", "Moviemiento Nacionalista Revolucionario", .)
df_1$partido_o_alianza %<>% gsub("PDC", "Partido Demócrata Cristino", .)
df_1$partido_o_alianza %<>% gsub("PAN-BOL", "Partido de Acción Nacional Boliviano", .)
df_1$partido_o_alianza %<>% gsub("FPV", "Frente para la Victoria", .)
df_1$partido_o_alianza %<>% gsub("21F", "Bolivia dijo No", .)
df_1$partido_o_alianza %<>% gsub("UCS", "Unión Cívica Solidaridad", .)

df_1$candidato_a_la_presidencia %<>% gsub("Morales", "Evo Morales", .)
df_1$candidato_a_la_presidencia %<>% gsub("Mesa", "Carlos Mesa", .)
df_1$candidato_a_la_presidencia %<>% gsub("Patzi", "Félix Patzi", .)
df_1$candidato_a_la_presidencia %<>% gsub("Lema", "Virginio Lema", .)
df_1$candidato_a_la_presidencia %<>% gsub("Quiroga", "Jorge Quiroga", .)
df_1$candidato_a_la_presidencia %<>% gsub("Paz Zamora", "Jaime Paz Zamora", .)
df_1$candidato_a_la_presidencia %<>% gsub("Nina", "Ruth Nina", .)
df_1$candidato_a_la_presidencia %<>% gsub("Rodríguez", "Israel Rodríguez", .)
df_1$candidato_a_la_presidencia %<>% gsub("Albarracín", "Waldo Albarracín", .)
df_1$candidato_a_la_presidencia %<>% gsub("Revilla", "Luis Revilla", .)
df_1$candidato_a_la_presidencia %<>% gsub("Costas", "Rubén Costas", .)
df_1$candidato_a_la_presidencia %<>% gsub("Doria Medina", "Samuel Doria Medina", .)
df_1$candidato_a_la_presidencia %<>% gsub("Ortiz", "Oscar Ortiz", .)
df_1$candidato_a_la_presidencia %<>% gsub("Cárdenas", "Victor Hugo Cárdenas", .)
df_1$candidato_a_la_presidencia %<>% gsub("Voto secreto", "El voto es secreto", .)



df_1$encuestadora %<>% gsub("_", " ", .)
df_1$encuestadora %<>% str_to_title(.)
df_1$encuestadora %<>% gsub("Mercados Y Muestras", "Mercados y Muestras", .)


df_1$medio %<>% gsub("pg7", "Página 7", .) 
df_1$medio %<>% gsub("pat", "PAT", .) 
df_1$medio %<>% gsub("rtp", "RTP", .) 
df_1$medio %<>% gsub("atb", "ATB", .) 
df_1$medio %<>% gsub("la_razon", "La Razón", .) 

# cambiar nombres de columnas, horizontalizar, correccion de valores
df_1 %<>% 
  select(-partido_o_alianza, -codigo_encuesta) %>% 
  spread(candidato_a_la_presidencia, valor) %>% 
  select(encuestadora, `Evo Morales`, `Carlos Mesa`, `Oscar Ortiz`, `Félix Patzi`, `Victor Hugo Cárdenas`, `Virginio Lema`,
         `Israel Rodríguez`, `Ruth Nina`, `Jaime Paz Zamora`, `Jorge Quiroga`, `Luis Revilla`, `Rubén Costas`, `Samuel Doria Medina`, 
         `Waldo Albarracín`, Otros, Indecisos, Blanco, Nulo, `Blanco/Nulo`, `El voto es secreto`, Ninguno, `No contesta`, 
         `NS/NR`, medio, alcance_muestra, margen_error, confianza, `ciudades_capitales (+EA)`, ciudades_intermedias, area_rural, 
         fecha_inicio_encuesta, fecha_conclusion_encuesta) %>% 
  rename(
    Encuestadora = encuestadora,
    `Medio de comunicación de difusión` = medio,
    `Márgen de error` = margen_error,
    Confianza = confianza,
    `Tamaño de la muestra` = alcance_muestra,
    `Ciudades capitales + El Alto` = `ciudades_capitales (+EA)`,
    `Ciudades intermedias` = ciudades_intermedias,
    `Localidades rurales` = area_rural,
    `Fecha de inicio de la encuesta` = fecha_inicio_encuesta,
    `Fecha de conclusión de la encuesta` = fecha_conclusion_encuesta
  ) %>% 
  mutate(
    `Márgen de error` = `Márgen de error` * 100,
    Confianza = Confianza * 100,
    `Fecha de inicio de la encuesta` = janitor::excel_numeric_to_date(`Fecha de inicio de la encuesta`),
    `Fecha de conclusión de la encuesta` = janitor::excel_numeric_to_date(`Fecha de conclusión de la encuesta`)
  ) %>% 
  arrange(`Fecha de conclusión de la encuesta`) 

# empujar a drive
df_1 %>% 
  rio::export("output_para_referencia/base_datos_drive.xlsx")





data %>% View

ew()data <- aggregate(. ~ Species, iris, toString)

reactable(data, wrap = T, resizable = TRUE)


devtools::install_github("glin/reactable")


  
  